{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.FPS
  ( Data
  , init
  , render
  , update
  ) where

import Control.Lens ((&), (^.), (%~), (.~), (?~), makeLenses)
import Data.IORef
import qualified Data.Text as T
import Prelude hiding (init)

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

type Data = IORef State

init :: Double -> IO Data
init _timeStart = do
  let _frameTotal = 0
      _fpsTotal = 0
      _intervalStart = _timeStart
      _frameInterval = 0
      _fpsLastInterval = Nothing
  newIORef State {..}

render :: NVG.Context -> Data -> IO ()
render ctx stRef = do
  st <- readIORef stRef

  NVG.fontFace ctx "Liberation Sans"
  NVG.fontSize ctx 16
  NVG.fillColor ctx $ NVG.Color 0 1 0 1
  NVG.text ctx 30 30 $ "FPS: " <> maybe "TBD" (T.pack . show) (st ^. fpsLastInterval)

update :: Double -> Data -> IO ()
update t stRef =
  modifyIORef' stRef $ \st -> (frameTotal %~ (+1))
                            . (fpsTotal .~ (fromIntegral (st ^. frameTotal + 1) / (t - st ^. timeStart))) $
    if t - st ^. intervalStart > 1
      then st & fpsLastInterval ?~ (st ^. frameInterval - 1)
              & frameInterval .~ 1
              & intervalStart .~ t
      else st & frameInterval %~ (+1)
