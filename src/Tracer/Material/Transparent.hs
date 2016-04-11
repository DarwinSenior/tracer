module Tracer.Material.Transparent where

import Tracer.Base
import Tracer.Vec

data Transparent = Transparent !Scalar

-- instance Material Transparent where
--   brdf Transparent _ _ _ = tovec 0
--   scatter Mirror (Intersection _ norm pos) (Ray _ indir) =
--     return $ [rayshift (Ray pos (reflect (-in_dir) norm))]
