{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.NanoVG.FPS
  ( showFPS
  ) where

import Control.Lens ((&), (^.), (%~), (.~), (?~), makeLenses)
import Data.IORef
import qualified Data.Text as T
import Prelude hiding (init)

import qualified Graphics.UI.GLFW as GLFW
import           Graphics.NanoVG.Window
import qualified NanoVG as NVG

data State = State
  { _timeStart       :: !Double
  , _frameTotal      :: !Int
  , _fpsTotal        :: !Double

  , _intervalStart   :: !Double
  , _frameInterval   :: !Int
  , _fpsLastInterval :: !(Maybe Int)
  }
makeLenses ''State

data Data = Data
  { fontAlias :: !T.Text
  , stRef :: !(IORef State)
  }

-- | This middleware shows FPS counter in top left corner of the window.
showFPS
  :: T.Text
  -- ^ Alias of the font to render text with. Refer to 'Graphics.NanoVG.Simple.loadFont' for more
  -> MiddleWare ((,) Data)
showFPS fontAlias Window {..} = Window
  { winInit = \ctx -> do
      Just time <- GLFW.getTime
      (,) <$> init fontAlias time <*> winInit ctx
  , winRender = \(d, st) ctx ->
      winRender st ctx *> render ctx d
  , winAfterRender = \(d, st) ctx -> do
      Just time <- GLFW.getTime
      update time d *> winAfterRender st ctx
  }

init :: T.Text -> Double -> IO Data
init fontAlias _timeStart = do
  let _frameTotal = 0
      _fpsTotal = 0
      _intervalStart = _timeStart
      _frameInterval = 0
      _fpsLastInterval = Nothing
  stRef <- newIORef State {..}
  pure Data {..}

render :: NVG.Context -> Data -> IO ()
render ctx Data {..} = do
  st <- readIORef stRef

  NVG.fontFace ctx fontAlias
  NVG.fontSize ctx 16
  NVG.fillColor ctx $ NVG.Color 0 1 0 1
  NVG.text ctx 30 30 $ "FPS: " <> maybe "TBD" (T.pack . show) (st ^. fpsLastInterval)

update :: Double -> Data -> IO ()
update t Data {..} =
  modifyIORef' stRef $ \st -> (frameTotal %~ (+1))
                            . (fpsTotal .~ (fromIntegral (st ^. frameTotal + 1) / (t - st ^. timeStart))) $
    if t - st ^. intervalStart > 1
      then st & fpsLastInterval ?~ (st ^. frameInterval - 1)
              & frameInterval .~ 1
              & intervalStart .~ t
      else st & frameInterval %~ (+1)
