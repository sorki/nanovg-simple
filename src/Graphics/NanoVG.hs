{-# LANGUAGE OverloadedLists #-}

module Graphics.NanoVG
  ( Context
  , init
  , runFrame

  , testRect
  ) where

import qualified NanoVG as NVG
import           Prelude hiding (init)

type Context = NVG.Context

init :: IO Context
init = NVG.createGL3 [NVG.Antialias, NVG.StencilStrokes, NVG.Debug]

runFrame :: Context -> Int -> Int -> IO a -> IO a
runFrame c w h act = NVG.beginFrame c (fromIntegral w) (fromIntegral h) 1 *> act <* NVG.endFrame c

testRect :: Context -> IO ()
testRect c = do
  NVG.beginPath c
  NVG.rect c 100 100 120 30
  NVG.fillColor c $ NVG.Color 0 1 0 1
  NVG.fill c
