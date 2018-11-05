-- | Description: Simple high-level context management interface for NanoVG with GLFW
-- Module provides utilities to create usable NanoVG contexts in GLFW windows.
module Graphics.NanoVG.Simple
  ( -- * Windows
    Window (..)
  , run
    -- * Middlewares
  , MiddleWare
  , showFPS
  , loadFont
  ) where

import Graphics.NanoVG.Window
import Graphics.NanoVG.FPS
import Graphics.NanoVG.Fonts
