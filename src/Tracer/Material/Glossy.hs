module Tracer.Material.Glossy where

import           Tracer.Base
import           Tracer.Material
import           Tracer.Ray
import           Tracer.Sampler
import           Tracer.Vec
import qualified Data.Vector as V

data Glossy = Glossy !Scalar

instance Material Glossy where
  brdf (Glossy _) = purebrdf (tovec 0)
  scatter (Glossy e) (Intersection _ norm pos) (Ray _ in_dir) = do
    let reflect_dir = reflect (-in_dir) norm
        intensity dir = (reflect_dir `dot` dir) ** e
        visible_ray = (> 0) . fst
        pair dir = (intensity dir, Ray pos dir)
        axis1 = normalize $ reflect_dir `cross` (tovec 1)
        axis2 = reflect_dir `cross` axis1
        toray (phi, theta) =
          let x = sin (theta * pi * 2)
              y = cos (theta * pi * 2)
              z = cos (phi * pi)
              w = sin (phi * pi)
          in normalize $ z *| reflect_dir + w *| (x *| axis1 + y *| axis2)
    return . V.toList . (V.filter visible_ray) . (V.map pair) . (V.map toray) $ sN 5
