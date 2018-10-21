module Main where

import           Control.Concurrent
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Loops
import           Foreign.C.Types
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))
import           System.IO (hPutStrLn, stderr)

import qualified Graphics.Rendering.FreeType as F
import qualified Graphics.Rendering.FreeType.OpenGL as F

import qualified Graphics.NanoVG as NVG
import qualified Graphics.FrameSize as FS
import qualified Graphics.FPS as FPS

foreign import ccall unsafe "initGlew"
  glewInit :: IO CInt

main :: IO ()
main = withGLFW $
  F.withFreeType $ \ft ->
  F.withFontFace ft "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf" $ \ff ->
    createWindow 800 600 "nanovg Playground" >>= go ff
  where
    go ff win = do
      GLFW.makeContextCurrent $ Just win
      void glewInit
      GLFW.swapInterval 0

      GL.rowAlignment GL.Unpack $= 1

      Just timeStart <- GLFW.getTime

      renderer <- F.newDelayedRenderer =<<
        F.newFontAtlas ff (F.Height <$> [10, 12, 14, 16, 18, 20, 24, 28, 32, 36, 40, 48])
      context <- NVG.init
      frameSize <- FS.init win
      fps <- FPS.init timeStart

      whileM_ (not <$> GLFW.windowShouldClose win) $ do
        throwError
        (width, height) <- FS.size frameSize

        GL.clear [GL.ColorBuffer]
        FPS.render renderer fps
        flushTexts renderer width height
        NVG.runFrame context width height $
          NVG.testRect context
        GLFW.swapBuffers win
        GL.flush
        GLFW.pollEvents

        Just timeAfter <- GLFW.getTime
        FPS.update timeAfter fps
    flushTexts renderer width height = do
      GL.blend $= GL.Enabled
      GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
      F.flushRender renderer width height

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
