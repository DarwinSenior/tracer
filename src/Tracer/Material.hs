module Tracer.Material where

import Tracer.Base
import Tracer.Vec

black :: Color
black = tovec 0

data BlackBody = BlackBody

instance Material BlackBody where
  brdf BlackBody _ _ _ = black
  scatter BlackBody _ _ = return []
