module Tracer.Material.Transparent where

import           Tracer.Base
import           Tracer.Ray
import           Tracer.Vec

data Transparent = Transparent !Scalar


instance Material Transparent where
  brdf _ _ _ _ _ = tovec 0
  scatter (Transparent ita) (Intersection _ norm pos) (Ray _ indir) =
    let (ita', norm') = if indir `dot` norm < 0 then (ita, norm) else (1/ita, -norm)
    in return $ case refract (-indir) norm' ita' of
               Nothing -> []
               Just newdir -> [rayshift (Ray pos newdir)]
  is_shadowable _ = False
