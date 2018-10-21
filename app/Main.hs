module Main where

import qualified Graphics.Window as Win

data App = App

main :: IO ()
main = Win.run 800 600 "nanovg Playground" Win.Window
  { Win.winInit = pure App
  , Win.winRender = \App _ -> pure ()
  , Win.winAfterRender = \App -> pure ()
  }
