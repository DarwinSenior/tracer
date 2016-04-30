module Tracer.Material.Transparent where

import           Tracer.Base
import           Tracer.Material
import           Tracer.Ray
import           Tracer.Vec

data Transparent = Transparent !Scalar

-- do note that the transparency has implicit state
-- the norm is always pointed to the outside so that
-- if the indir and norm are in the same direction means
-- the ray is inside the material, so that the index should
-- thus, the index(ita) is then reverse
instance Material Transparent where
  brdf (Transparent _) = purebrdf (tovec 0)
  scatter (Transparent ita) (Intersection _ norm pos) (Ray _ indir) =
    let (ita', norm') = if indir `dot` norm < 0 then (ita, norm) else (1/ita, -norm)
    in return $ case refract (-indir) norm' ita' of
               Nothing -> []
               Just newdir -> [(1, rayshift (Ray pos newdir))]
