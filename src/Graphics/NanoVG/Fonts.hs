{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.NanoVG.Fonts
  ( loadFont
  ) where

import Control.Monad
import Data.Functor.Identity
import Data.Text (Text)
import Graphics.NanoVG.Window
import qualified NanoVG as NVG

-- | Load font from of the supported file formats and store it as given alias for further use.
loadFont
  :: Text
  -- ^ File path to load font from. Font is loaded with freetype2 so refer to it's documentation for allowed formats.
  -> Text
  -- ^ Alias which can later be used to refer to the loaded font.
  -> MiddleWare Identity
loadFont file alias Window {..} = Window
  { winInit = \ctx -> do
      void $ NVG.createFont ctx alias $ NVG.FileName file
      Identity <$> winInit ctx
  , winRender = winRender . runIdentity
  , winAfterRender = winAfterRender . runIdentity
  }
