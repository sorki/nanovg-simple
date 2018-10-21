module Main where

import           Control.Concurrent
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Loops
import           Foreign.C.Types
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import           System.IO (hPutStrLn, stderr)

import qualified Graphics.NanoVG as NVG
import qualified Graphics.FrameSize as FS
import qualified Graphics.FPS as FPS

foreign import ccall unsafe "initGlew"
  glewInit :: IO CInt

main :: IO ()
main = withGLFW $
  createWindow 800 600 "nanovg Playground" >>= go
  where
    go win = do
      GLFW.makeContextCurrent $ Just win
      void glewInit

      -- Leave it up to vblank_mode / __GL_SYNC_TO_VBLANK
      -- GLFW.swapInterval 0

      Just timeStart <- GLFW.getTime

      ctx <- NVG.init
      frameSize <- FS.init win
      fps <- FPS.init timeStart

      whileM_ (not <$> GLFW.windowShouldClose win) $ do
        throwError
        (width, height) <- FS.size frameSize

        GL.clear [GL.ColorBuffer]
        NVG.runFrame ctx width height $
          FPS.render ctx fps
        GLFW.swapBuffers win
        GL.flush
        GLFW.pollEvents

        Just timeAfter <- GLFW.getTime
        FPS.update timeAfter fps

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
