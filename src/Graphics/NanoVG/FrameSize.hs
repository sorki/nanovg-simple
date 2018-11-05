{-# LANGUAGE RecordWildCards #-}

module Graphics.NanoVG.FrameSize
  ( Data
  , init
  , size
  ) where

import Data.Functor
import Data.IORef
import Prelude hiding (init)

import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

data State = State
  { width :: !Int
  , height :: !Int
  }

type Data = IORef State

init :: GLFW.Window -> IO Data
init win = do
  stRef <- do
    (width, height) <- GLFW.getFramebufferSize win
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
    newIORef State {..}

  GLFW.setFramebufferSizeCallback win $ Just $ \_ width height -> do
    writeIORef stRef State {..}
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
  pure stRef

size :: Data -> IO (Int, Int)
size stRef = readIORef stRef <&> \State {..} -> (width, height)
