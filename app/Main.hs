{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time.Clock.POSIX
import qualified Graphics.NanoVG.Simple as NS
import qualified Graphics.NanoVG.Picture as P
import qualified NanoVG as NVG

main :: IO ()
main = do
  start <- getPOSIXTime
  NS.run 800 600 "nanovg Playground" $
    NS.showFPS "Liberation Sans" $
    NS.loadFont "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf" "Liberation Sans" $
    P.runWindow $ getPOSIXTime >>= \time -> pure $
      P.rotateP (400, 300) (realToFrac $ time - start) $
      P.scaleP' (0, 0) 10 $
        P.pictures
          [ P.translateP (12 * (x+1)) (12 * (y+1)) myRing
          | x <- [0..4]
          , y <- [0..3]
          ]
  where
    myRing = P.fill (NVG.Color 1 0 0 1) $ P.shapes
      [ P.circle (0, 0) 4
      , P.hole $ P.circle (0, 0) 3
      ]
