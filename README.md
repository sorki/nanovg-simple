# nanovg-simple

Simple interface to creating window with associated NanoVG context. See
[nanovg.h](https://github.com/memononen/nanovg/blob/master/src/nanovg.h) for comprehensive listing of methods.

Refer to `Graphics.NanoVG.Simple` module for utilities to create NanoVG window. Simple example:

```haskell
import           Graphics.NanoVG.Simple
import qualified NanoVG as NVG

main :: IO ()
main = run 800 600 "Simple app" $ simpleWindow $
  NVG.circle ctx 10 10 10 *> NVG.fill ctx
```

Also provided is wrapper for rendering combination of composable picture pieces: see `Graphics.NanoVG.Picture`.

```haskell
import           Graphics.NanoVG.Picture
import           Graphics.NanoVG.Simple

main :: IO ()
main = run 800 600 "Simple app" $ asWindow $
  pure $ translateP 50 0 $ mconcat
    [ fill (Color 1 1 1 1) $ circle (10, 10) 10
    , stroke (Color 1 1 1 1) $ circle (10, 10) 15
    , fill (Color 0 1 0 1) $ $ translateS (-50) 0 $ line (0, 0) (5, 5)
    ]
```
