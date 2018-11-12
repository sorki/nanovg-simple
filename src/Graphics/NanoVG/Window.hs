{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.NanoVG.Window
  ( Window (..)
  , MiddleWare
  , run
  ) where

import           Control.Concurrent
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Loops
import           Foreign.C.Types
import           System.IO (hPutStrLn, stderr)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified NanoVG as NVG

import qualified Graphics.NanoVG.FrameSize as FS

foreign import ccall unsafe "initGlew"
  glewInit :: IO CInt

-- | Window keep state and repeatedly calls render/afterRender.
--
-- There is no interface to update state directly so mutable containers should be used, if so desired.
data Window st = Window
  { winInit        :: !(NVG.Context -> IO st)
  , winRender      :: !(st -> NVG.Context -> IO ())
  , winAfterRender :: !(st -> NVG.Context -> IO ())
  }

-- | Middleware adds some piece of functionality to existing window.
type MiddleWare st = forall st0. Window st0 -> Window (st st0)

-- | Run given rendering instructions ('Window' structure) in new GLFW window of given size.
run
  :: Int
  -- ^ Initial window width.
  -> Int
  -- ^ Initial window height.
  -> String
  -- ^ Window title.
  -> Window st
  -- ^ Rendering instructions to be executed in the context of the new window.
  -> IO ()
run initWidth initHeight title Window {..} = withGLFW $
  createWindow initWidth initHeight title >>= go
  where
    go win = do
      GLFW.makeContextCurrent $ Just win
      void glewInit

      -- Leave it up to vblank_mode / __GL_SYNC_TO_VBLANK
      -- GLFW.swapInterval 0

      ctx <- NVG.createGL3 [NVG.Antialias, NVG.StencilStrokes, NVG.Debug]
      frameSize <- FS.init win

      st <- winInit ctx

      whileM_ (not <$> GLFW.windowShouldClose win) $ do
        throwError
        (width, height) <- FS.size frameSize

        GL.clear [GL.ColorBuffer, GL.StencilBuffer]
        runFrame ctx width height $ winRender st ctx
        GLFW.swapBuffers win
        GL.flush
        GLFW.pollEvents

        winAfterRender st ctx

    runFrame :: NVG.Context -> Int -> Int -> IO a -> IO a
    runFrame c w h act = NVG.beginFrame c (fromIntegral w) (fromIntegral h) 1 *> act <* NVG.endFrame c

throwError :: IO ()
throwError = do
  errs <- GL.errors
  case errs of
    err:_ ->
      throwString $ "OpenGL error: " <> show err
    _     ->
      pure ()


createWindow :: Int -> Int -> String -> IO GLFW.Window
createWindow w h title = do
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
  Just win <- GLFW.createWindow w h title Nothing Nothing
  pure win


-- | Do NOT try to nest calls!
-- TODO: use global flag (via unsafePerformIO) to seemlessly handle nesting.
withGLFW :: IO a -> IO a
withGLFW act = runInBoundThread $ do
  GLFW.setErrorCallback $ Just $ \err msg ->
    hPutStrLn stderr $ "GLFW error: " <> show err <> "\n" <> msg
  bracket_ (assertTrueM GLFW.init) GLFW.terminate act
  where
    assertTrueM predM = do
      True <- predM
      pure ()
