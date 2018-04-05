module Shared.MathUtils where

import Prelude

import Math as Math

deg2Rad :: Number -> Number
deg2Rad x = x * Math.pi / 180.0

rad2Deg :: Number -> Number
rad2Deg x = x * 180.0 / Math.pi

angleBetween :: forall r1 r2.
  { point1 :: { x :: Number, y :: Number | r1 }
  , point2 :: { x :: Number, y :: Number | r2 }
  } ->
  Number
angleBetween { point1: { x: x1, y: y1}, point2: { x: x2, y: y2} } =
  Math.atan2 (y2 - y1) (x2 - x1)

wrapAngle :: Number -> Number
wrapAngle angle = angle # clamp (-180.0) 180.0

distance :: forall r1 r2.
  { point1 :: { x :: Number, y :: Number | r1 }
  , point2 :: { x :: Number, y :: Number | r2 }
  } ->
  Number
distance { point1: { x: x1, y: y1}, point2: { x: x2, y: y2} } =
  let
    dx = x1 - x2
    dy = y1 - y2
  in Math.sqrt (dx * dx + dy * dy)
