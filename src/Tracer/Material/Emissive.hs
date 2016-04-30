module Tracer.Material.Emissive where

import Tracer.Base
import Tracer.Material

data Emissive = Emissive !Color

instance Material Emissive where
  brdf (Emissive color) = purebrdf color
  scatter _ _ _ = return []
  is_shadowable (Emissive _) = False
