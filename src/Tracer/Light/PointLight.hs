{-# LANGUAGE BangPatterns #-}
module Tracer.Light.PointLight where

import Tracer.Base
import Tracer.Vec
import Tracer.Light (shadelight)

data PointLight = PointLight !Pos !Color

instance Light PointLight where
  shade light = shadelight $ pointlightfunc light

pointlightfunc :: PointLight -> Pos -> (Scalar, Dir, Color)
pointlightfunc (PointLight pos colour) pos' = (dist, dir, colour)
  where
    !dpos = pos - pos'
    !dir = normalize dpos
    !dist = vmag dpos
