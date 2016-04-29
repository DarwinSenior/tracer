module Tracer.Texture.MonoColor where

import Tracer.Base

newtype MonoColor = MonoColor Color

instance Texture MonoColor where
  texture (MonoColor c) _ = c
