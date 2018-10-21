module Main where

import           Control.Concurrent
import           Control.Exception.Safe
import           Control.Monad.Loops
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL (errors)
import           System.IO (hPutStrLn, stderr)


main :: IO ()
main = withGLFW $
  createWindow 800 600 "nanovg Playground" >>= go
  where
    go win = do
      GLFW.makeContextCurrent $ Just win
      GLFW.swapInterval 0
      whileM_ (not <$> GLFW.windowShouldClose win) $ do
        throwError
        GLFW.swapBuffers win
        GLFW.pollEvents

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
