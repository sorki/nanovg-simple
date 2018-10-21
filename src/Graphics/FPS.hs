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
import Linear (V2 (..), V4 (..))
import Prelude hiding (init)

import qualified Graphics.Rendering.FreeType.OpenGL as F
import qualified Graphics.Rendering.FreeType as F

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

render :: F.DelayedRenderer -> Data -> IO ()
render writer stRef = do
  st <- readIORef stRef
  F.enqueue writer (V2 30 30) $
    F.textPart (V4 0 1 0 1) (F.Height 16) $ "FPS: " <> maybe "TBD" show (st ^. fpsLastInterval)

update :: Double -> Data -> IO ()
update t stRef = do
  st' <- readIORef stRef
  modifyIORef' stRef $ \st -> (frameTotal %~ (+1))
                            . (fpsTotal .~ (fromIntegral (st ^. frameTotal + 1) / (t - st ^. timeStart))) $
    if t - st ^. intervalStart > 1
      then st & fpsLastInterval ?~ (st ^. frameInterval - 1)
              & frameInterval .~ 1
              & intervalStart .~ t
      else st & frameInterval %~ (+1)
  if t - st' ^. intervalStart > 1 then print $ st' ^. fpsLastInterval
                                  else pure ()
