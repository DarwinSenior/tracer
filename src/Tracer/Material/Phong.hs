{-# LANGUAGE BangPatterns #-}
module Tracer.Material.Phong where

import           Tracer.Base
import           Tracer.Vec
import           Tracer.Ray

-- Phong color kd (diffusive) ks (specular) alpha
data Phong tex = Phong !tex !Scalar !Scalar !Scalar

instance (Texture tex) => Material (Phong tex) where
  brdf (Phong tex kd ks alpha) indir norm outdir pos =
    let ref_dir = reflect outdir norm
        !diffusive = kd * max 0 (outdir `dot` norm)
        !specular = ks * (max 0 (indir `dot` ref_dir)) ** alpha
        !color = texture tex pos
    in color |* (diffusive + specular)
  scatter _ _ _ = return []
