module Tracer.Screen.Pinhole where

import           Tracer.Base
import           Tracer.Vec
import           Tracer.Intersection

-- Pinhole focus width height
data Pinhole = Pinhole !Scalar !Scalar !Scalar

shoot :: Pinhole -> Camera -> (Scalar, Scalar) -> Ray
shoot (Pinhole focus width height) cam (rx, ry) =
  let Camera pos up front = cam
      right = up `cross` front
      dx = right |* (width * (rx - 0.5))
      dy = -up |* (height * (ry - 0.5))
      o = pos + dx + dy
      d = if focus < inf
            then normalize (o - pos + (front |* focus))
            else front
  in (Ray o d)

instance Screen Pinhole where
  shootRay pinhole cam pos = return [shoot pinhole cam pos]
