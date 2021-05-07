module Geometry.Cuboid
  ( volume
  , area
  ) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectArea a b * c + rectArea b c * a + rectArea c a * b

rectArea :: Float -> Float -> Float
rectArea a b = a * b
