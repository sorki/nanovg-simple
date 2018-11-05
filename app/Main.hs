{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Graphics.NanoVG.Simple as NS
import qualified NanoVG as NVG

data App = App

main :: IO ()
main = NS.run 800 600 "nanovg Playground" $
  NS.showFPS "Liberation Sans" $
  NS.loadFont "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf" "Liberation Sans"
  NS.Window
    { NS.winInit = \_ -> pure App
    , NS.winRender = \App ctx -> do
        NVG.beginPath ctx
        forM_ [0..4] $ \x ->
          forM_ [0..3] $ \y -> do
            NVG.strokeColor ctx $ NVG.Color 1 0 0 1
            NVG.strokeWidth ctx 10
            NVG.circle ctx (120 * (x + 1)) (120 * (y + 1)) 40
        NVG.stroke ctx
    , NS.winAfterRender = \App _ -> pure ()
    }
