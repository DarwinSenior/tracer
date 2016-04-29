module Tracer.Material.Emissive where

import Tracer.Base

data Emissive = Emissive !Color

instance Material Emissive where
  brdf (Emissive color) _ _ _ _ = color
  scatter _ _ _ = return []
  is_shadowable (Emissive _) = False
