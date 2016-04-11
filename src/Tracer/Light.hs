module Tracer.Light where

import           Tracer.Base
import           Tracer.Vec
import           Tracer.Intersection (inf)
import           Control.Monad.Reader
import           Tracer.Material (black)
import           Tracer.Scene

-- shoot the ray to the light source
ifShadow :: Ray -> Scalar -> Color -> SceneM Color
ifShadow ray lightdist color = do
  use_shadow <- asks $ _use_shadow . snd
  if use_shadow
    then do
      is_in_shadow <- inshadow ray lightdist
      return
        (if is_in_shadow
           then black
           else color)
    else return color

-- helper function calculate shading given where the intersection and light composition
-- it has shadow option and return black for inifinite intersection
shadelight :: (Pos -> (Scalar, Dir, Color)) -> Scatter -> SceneM Color
shadelight f scattered = do
  case scattered of
    Scatter (Ray _ indir) (Intersection dist norm pos) mat ->
      let (lightdist, outdir, color) = f pos
          ray = Ray pos outdir
          shadecolor = (brdf mat (-indir) norm outdir) * color
      in if dist < inf
           then ifShadow ray lightdist shadecolor
           else return black
