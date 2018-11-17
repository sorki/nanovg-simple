-- | Description: Simple high-level context management interface for NanoVG with GLFW
-- Module provides utilities to create usable NanoVG contexts in GLFW windows.
module Graphics.NanoVG.Simple
  ( -- | Simple example:
    --
    -- @
    -- import           Graphics.NanoVG.Simple
    -- import qualified NanoVG as NVG
    --
    -- main :: IO ()
    -- main = run 800 600 "Simple app" $ simpleWindow $
    --   NVG.circle ctx 10 10 10 *> NVG.fill ctx
    -- @

    -- * Windows
    Window (..)
  , simpleWindow
  , run
    -- * Middlewares
  , MiddleWare
  , showFPS
  , loadFont
  ) where

import Graphics.NanoVG.Window
import Graphics.NanoVG.FPS
import Graphics.NanoVG.Fonts
