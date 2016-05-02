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
        normalize' x =
          let total = V.sum $ V.map fst x
              update (coeff, ray) = (coeff/total, ray)
          in V.map update x
        pair dir = (intensity dir, rayshift $ Ray pos dir)
        axis1 = normalize $ reflect_dir `cross` (tovec 1)
        axis2 = reflect_dir `cross` axis1
        toray (x, y) =
          normalize $ reflect_dir + (2 * x - 1) *| axis1 + (2 * y - 1) *| axis2
    return . V.toList . normalize' . (V.filter visible_ray) . (V.map pair) . (V.map toray) $ sN 10
