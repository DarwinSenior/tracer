module Tracer.Texture.Checker where

import           Tracer.Base
import           Tracer.Geometry
import           Tracer.Vec

data Checker g = Checker g Float

instance (Surfceable g) => Texture (Checker g) where
  texture (Checker geo fraction) pos =
    let (x, y) = surface' geo pos
        is_white = (floor (x * fraction) `mod` 2 == 0) /= (floor (y * fraction) `mod` 2 == 0)
    in if is_white then tovec 1 else tovec 0
