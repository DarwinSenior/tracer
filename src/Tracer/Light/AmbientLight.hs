module Tracer.Light.AmbientLight where

import           Tracer.Base
import           Tracer.Intersection
import           Tracer.Material (black)

data AmbientLight = AmbientLight !Color

instance Light AmbientLight where
  shade (AmbientLight color) scattered =
    case scattered of
      Scatter _ inter _ ->
        if inter < farest_intersection
          then return color
          else return black
