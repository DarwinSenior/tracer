module Tracer.Material where

import Tracer.Base
import Tracer.Vec

black :: Color
black = tovec 0

data BlackBody = BlackBody

purebrdf :: Color -> Dir -> Dir -> Dir -> Pos -> Color
purebrdf color _ _ _ _ = color

instance Material BlackBody where
  brdf BlackBody = purebrdf (tovec 0)
  scatter BlackBody _ _ = return []
