{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.NanoVG.FPS
  ( showFPS
  ) where

import Data.IORef
import qualified Data.Text as T
import Prelude hiding (init)

import qualified Graphics.UI.GLFW as GLFW
import           Graphics.NanoVG.Window
import qualified NanoVG as NVG

data State = State
  { timeStart       :: !Double
  , frameTotal      :: !Int
  , fpsTotal        :: !Double

  , intervalStart   :: !Double
  , frameInterval   :: !Int
  , fpsLastInterval :: !(Maybe Int)
  }

data Data = Data
  { fontAlias :: !T.Text
  , stRef :: !(IORef State)
  }

-- | This middleware shows FPS counter in top left corner of the window.
showFPS
  :: T.Text
  -- ^ Alias of the font to render text with. Refer to 'Graphics.NanoVG.Simple.loadFont' for more
  -> MiddleWare a (Data, a)
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
init fontAlias timeStart = do
  let frameTotal = 0
      fpsTotal = 0
      intervalStart = timeStart
      frameInterval = 0
      fpsLastInterval = Nothing
  stRef <- newIORef State {..}
  pure Data {..}

render :: NVG.Context -> Data -> IO ()
render ctx Data {..} = do
  st <- readIORef stRef

  NVG.fontFace ctx fontAlias
  NVG.fontSize ctx 16
  NVG.fillColor ctx $ NVG.Color 0 1 0 1
  NVG.text ctx 30 30 $ "FPS: " <> maybe "TBD" (T.pack . show) (fpsLastInterval st)

update :: Double -> Data -> IO ()
update t Data {..} =
  modifyIORef' stRef $ \st ->
    let st' = if t - intervalStart st > 1
                 then st { fpsLastInterval = Just (frameInterval st - 1)
                         , frameInterval = 1
                         , intervalStart = t
                         }
                 else st { frameInterval = frameInterval st + 1
                         }
    in st' { frameTotal = frameTotal st + 1
           , fpsTotal = fromIntegral (frameTotal st + 1) / (t - timeStart st)
           }
