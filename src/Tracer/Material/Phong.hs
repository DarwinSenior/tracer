{-# LANGUAGE BangPatterns #-}
module Tracer.Material.Phong where

import           Tracer.Base
import           Tracer.Vec
import           Tracer.Ray

-- Phong color kd (diffusive) ks (specular) alpha
data Phong = Phong !Color !Scalar !Scalar !Scalar

instance Material Phong where
  brdf (Phong color kd ks alpha) indir norm outdir =
    let ref_dir = reflect outdir norm
        !diffusive = kd * max 0 (outdir `dot` norm)
        !specular = ks * (max 0 (indir `dot` ref_dir)) ** alpha
    in color |* (diffusive + specular)
  scatter _ _ _ = return []
