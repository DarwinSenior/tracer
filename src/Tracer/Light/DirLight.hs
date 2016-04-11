module Tracer.Light.DirLight (DirLight(..)) where

import           Tracer.Vec
import           Tracer.Base
import           Tracer.Intersection (inf)
import           Tracer.Light (shadelight)

-- directional light has a position and a color
data DirLight = DirLight !Dir !Color

instance Light DirLight where
  shade light =
    shadelight $ dirlightfunc light

dirlightfunc :: DirLight -> Pos -> (Scalar, Dir, Color)
dirlightfunc (DirLight dir colour) _ = (inf, -dir, colour)
