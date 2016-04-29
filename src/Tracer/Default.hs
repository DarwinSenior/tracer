module Tracer.Default where

import           Tracer.Base
import           Tracer.Geometry.Sphere
import           Tracer.Geometry.Triangle
import           Tracer.Light.DirLight
import           Tracer.Material.Phong
import           Tracer.Screen.Pinhole
import           Tracer.Texture.MonoColor
import           Tracer.Vec

-- this module will have some default value so that I could play around in ghci, it is not typically
-- included inside the body the same is very simple
camera :: Camera
camera = Camera (tovec 0) (Vector3 0 0 1) (Vector3 0 1 0)

setting :: Setting
setting = Setting
  { _maxraydepth = 2
  , _height = 200
  , _width = 200
  , _maxraysample = 100
  , _use_shadow = True
  , _maxlightsample = 100
  }

triangle :: Triangle
triangle = create_triangle (Vector3 0 0 5) (Vector3 1 1 5) (Vector3 0 1 5)

sphere :: Sphere
sphere = Sphere (Vector3 0 0 5) 2

pinhole :: Pinhole
pinhole = Pinhole 2.0 2.0 2.0

white :: Color
white = tovec 1

green :: Color
green = Vector3 0 1 0

dirlight :: DirLight
dirlight = DirLight (Vector3 0 0 1) green

phong :: Phong MonoColor
phong = Phong (MonoColor green) 0.5 0.5 1

scene :: Scene
scene = Scene camera pinhole [Light' dirlight] [Object sphere phong]

intersection :: Intersection
intersection = Intersection 1 dir0 pos0

pos0 :: Pos
pos0 = Vector3 0 0 0

dir0 :: Dir
dir0 = Vector3 0 0 1

ray0 :: Ray
ray0 = Ray pos0 dir0
