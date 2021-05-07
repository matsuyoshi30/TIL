module Geometry.Sphere -- ディレクトリ名.ファイル名として階層構造をもつモジュールとなる
  ( volume
  , area
  ) where

volume :: Float -> Float
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)

area :: Float -> Float
area radius = 4.0 * pi * (radius ^ 2)
