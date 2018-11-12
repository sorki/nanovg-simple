{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Graphics.NanoVG.Simple as NS
import qualified Graphics.NanoVG.Picture as P
import qualified NanoVG as NVG

main :: IO ()
main = NS.run 800 600 "nanovg Playground" $
  NS.showFPS "Liberation Sans" $
  NS.loadFont "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf" "Liberation Sans" $
  P.runWindow $ pure $
    P.pictures
      [ P.translateP myRing (120 * (x+1)) (120 * (y+1))
      | x <- [0..4]
      , y <- [0..3]
      ]
  where
    myRing = P.fill (NVG.Color 1 0 0 1) $ P.shapes
      [ P.circle (0, 0) 40
      , P.hole $ P.circle (0, 0) 30
      ]
