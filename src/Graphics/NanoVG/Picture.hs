{-# LANGUAGE LambdaCase #-}
-- | Description: Rendering composable picture fragments
-- The module was originally inspired by gloss Picture data type.
--
-- It allows you to define simple pieces, combine and move\/rotate\/scale them
-- to produce the final image.
module Graphics.NanoVG.Picture
  ( -- * Shapes
    Shape (..)

    -- ** Helper type aliases
  , Radius
  , Point
  , Center
  , Angle

    -- ** Predefined Constructors
  , circle
  , line
  , rectangle
  , arc
  , shapes
  , translateS
  , rotateS
  , scaleS
  , hole

    -- * Pictures
  , Picture

    -- * Contstructors

    -- | Pictures are generally constructed by filling or stroking shape
    -- or by transforming exising picture.

  , stroke
  , fill
  , pictures
  , translateP
  , rotateP
  , scaleP

    -- * Rendering
  , render

    -- * Rendering as 'Window'
  , runWindow
  ) where

import Control.Exception.Safe
import Data.Foldable
import Graphics.NanoVG.Window (Window (..))
import qualified NanoVG as NVG

-- | Radius of a circle or an arc.
type Radius = Float

-- | Point on 2D plane.
type Point = (Float, Float)

-- | Point representing center of circle, arc, rotation or scale.
type Center = Point

-- | Angle of rotation, arc, etc.
type Angle = Float

-- | Shape of a future picture fragment, to be filled or stroked later.
-- Action should define set of paths for the passed 'NVG.Context'.
newtype Shape = Shape
  { unShape :: NVG.Context -> IO ()
  }

runShape :: NVG.Context -> Shape -> IO ()
runShape = flip unShape

-- | Saves NanoVG state, applies modifications (first argument),
-- runs actions (second argument) and restores state.
-- TODO expose?
withState :: NVG.Context -> IO () -> IO () -> IO ()
withState ctx t = bracket_ (NVG.save ctx *> t) (NVG.restore ctx)

-- | Make circular shape.
circle :: Center -> Radius -> Shape
circle (x, y) r = Shape $ \ctx -> NVG.circle ctx (realToFrac x) (realToFrac y) (realToFrac r)

-- | Make line shape.
line :: Point -> Point -> Shape
line (ax, ay) (bx, by) = Shape $ \ctx -> do
  NVG.moveTo ctx (realToFrac ax) (realToFrac ay)
  NVG.lineTo ctx (realToFrac bx) (realToFrac by)

-- | Make rectangular shape given two (opposite) corners positions.
rectangle :: Point -> Point -> Shape
rectangle (ax, ay) (bx, by) = Shape $ \ctx ->
  NVG.rect ctx (realToFrac $ min ax bx) (realToFrac $ min ay by)
               (realToFrac $ abs $ ax - bx) (realToFrac $ abs $ ay - by)

-- | Make arc shape with given center and going counter-clockwise
-- from first angle to the second.
arc :: Center -> Radius -> Angle -> Angle -> Shape
arc (x, y) r a0 a1 = Shape $ \ctx ->
  NVG.arc ctx (realToFrac x) (realToFrac y) (realToFrac r) (realToFrac a0) (realToFrac a1) NVG.CCW

-- | Combine multiple shapes together.
shapes :: [Shape] -> Shape
shapes ss = Shape $ \ctx -> traverse_ (runShape ctx) ss

-- | Translate shape by given @x@ and @y@ offsets.
translateS :: Shape -> Float -> Float -> Shape
translateS s x y = Shape $ \ctx ->
  withState ctx (NVG.translate ctx (realToFrac x) (realToFrac y)) $
    unShape s ctx

-- | Rotate shape around given point by given angle.
rotateS :: Shape -> Center -> Angle -> Shape
rotateS s (x, y) a = Shape $ \ctx ->
  withState ctx
    (NVG.translate ctx fx fy *> NVG.rotate ctx fa *> NVG.translate ctx (-fx) (-fy))
    (unShape s ctx)
  where
    (fx, fy, fa) = (realToFrac x, realToFrac y, realToFrac a)

-- | Scale shape around given point by given factor.
scaleS :: Shape -> Center -> Float -> Shape
scaleS s (x, y) k = Shape $ \ctx ->
  withState ctx
    (NVG.translate ctx fx fy *> NVG.scale ctx fk fk *> NVG.translate ctx (-(fk*fx)) (-(fk*fy)))
    (unShape s ctx)
  where
    (fx, fy, fk) = (realToFrac x, realToFrac y, realToFrac k)

-- | Turns shape into a hole which can then be combined
-- with other (solid) shape. E.g.
--
-- > fill (Color 1 0 0 1) $ shapes [circle (0, 0) 40, hole $ circle (0, 0) 30]
--
-- can be used to create a ring of width 10.
hole :: Shape -> Shape
hole s = Shape $ \ctx -> withState ctx (NVG.pathWinding ctx $ fromIntegral $ fromEnum NVG.CW) $
  unShape s ctx

-- | Picture represent collection of filled/stroked shapes
-- ready to be rendered
data Picture =
    Stroke NVG.Color Shape
  | Fill NVG.Color Shape
  | Pictures [Picture]

-- | TODO expose?
mapShape :: (Shape -> Shape) -> Picture -> Picture
mapShape f = \case
  Stroke c s -> Stroke c $ f s
  Fill c s   -> Fill c $ f s
  Pictures ss  -> Pictures $ mapShape f <$> ss

-- | Translate the picture by given @x@ and @y@ offsets.
translateP :: Picture -> Float -> Float -> Picture
translateP p x y = mapShape (\s -> translateS s x y) p

-- | Rotate the picture around given point for given angle.
rotateP :: Picture -> Center -> Angle -> Picture
rotateP p c a = mapShape (\s -> rotateS s c a) p

-- | Scale the picture around given point by given factor.
scaleP :: Picture -> Center -> Float -> Picture
scaleP p c k = mapShape (\s -> scaleS s c k) p

-- | Stroke the shape to create a picture.
stroke :: NVG.Color -> Shape -> Picture
stroke = Stroke

-- | Fill the shape to create a picture.
fill :: NVG.Color -> Shape -> Picture
fill = Fill

-- | Combine multiple pictures together.
pictures :: [Picture] -> Picture
pictures = Pictures

-- | Render picture with given NanoVG context.
render :: NVG.Context -> Picture -> IO ()
render ctx = \case
  Stroke col s -> do
    NVG.beginPath ctx
    withState ctx (NVG.strokeColor ctx col *> NVG.strokeWidth ctx 1) $
      unShape s ctx *> NVG.stroke ctx
  Fill col s -> do
    NVG.beginPath ctx
    withState ctx (NVG.fillColor ctx col) $
      unShape s ctx *> NVG.fill ctx
  Pictures ss ->
    traverse_ (render ctx) ss

-- | Create 'Window' which constantly queries and renders received picture.
runWindow :: IO Picture -> Window ()
runWindow g = Window
  { winInit = \_ -> pure ()
  , winRender = \_ ctx -> g >>= render ctx
  , winAfterRender = \_ _ -> pure ()
  }
