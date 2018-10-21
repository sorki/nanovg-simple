{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.NanoVG
  ( Context
  , init
  , runFrame
  ) where

import           Control.Monad
import qualified NanoVG as NVG
import           Prelude hiding (init)

type Context = NVG.Context

init :: IO Context
init = do
  ctx <- NVG.createGL3 [NVG.Antialias, NVG.StencilStrokes, NVG.Debug]
  void $ NVG.createFont ctx "Liberation Sans" $
    NVG.FileName "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
  pure ctx

runFrame :: Context -> Int -> Int -> IO a -> IO a
runFrame c w h act = NVG.beginFrame c (fromIntegral w) (fromIntegral h) 1 *> act <* NVG.endFrame c
