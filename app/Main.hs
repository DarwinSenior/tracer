module Main where

import           Tracer.Render
import           Tracer.Base
import           Tracer.Vec
import           Tracer.Geometry.Sphere
import           Tracer.Geometry.Triangle
import           Tracer.Geometry.BHV
import Tracer.Geometry.Circle
import           Tracer.Geometry.Plane
import           Tracer.Material.Phong
import           Tracer.Material.Emissive
import           Tracer.Material.Mirror
import           Tracer.Light.DirLight
import           Tracer.Light.PointLight
import           Tracer.Light.AmbientLight
import Tracer.Light.AreaLight
import           Tracer.Screen.Pinhole
import           Tracer.Camera
import qualified Data.Vector as V
import           Codec.Picture.Png

setting :: Setting
setting = Setting
  { _maxraydepth = 3
  , _height = 500
  , _width = 500
  , _maxraysample = 1
  , _use_shadow = True
  , _maxlightsample = 20
  }

pinhole :: Pinhole
pinhole = Pinhole 20 10 10

camera :: Camera
camera = create_camera (tovec 0) (Vector3 0 (-1) 0) (Vector3 0 0 1)

camera' :: Camera
camera' = (yaw 0.5) . (roll 0.5) $ camera

circle :: Circle
circle = Circle (Vector3 0 (-3) 4) (normalize $ Vector3 0 (-1) 0) 2

arealight :: AreaLight Circle
arealight = AreaLight circle (Vector3 1 1 1)

dirlight :: DirLight
dirlight = DirLight (normalize $ Vector3 (-1) 1 0) (tovec 0.5)

dirlight2 :: DirLight
dirlight2 = DirLight (normalize $ Vector3 1 (-1) 0) (tovec 0.5)

pointlight :: PointLight
pointlight = PointLight (Vector3 0 (-3) 4) (Vector3 1 1 1)

ambientlight :: AmbientLight
ambientlight = AmbientLight (tovec 0.1)

plane :: Plane
plane = Plane (normalize $ Vector3 (0) (0) (-1)) (-20)

plane2 :: Plane
plane2 = Plane (Vector3 0 (-1) 0) (-5)

sphere :: Sphere
sphere = Sphere (Vector3 0 0 5) 2

triangle :: Triangle
triangle = create_triangle (Vector3 0 0 5) (Vector3 0 1 5) (Vector3 1 0 5)

phong :: Phong
phong = Phong (Vector3 0 1 1) 1 0 1

phong2 :: Phong
phong2 = Phong (Vector3 1 0 0) 0.9 0.1 1

scene :: Scene
scene = Scene
          camera'
          pinhole
          [Light' arealight]
          [Object plane phong, Object plane2 phong2, Object spheres mirror, Object circle emmisive]

emmisive :: Emissive
emmisive = Emissive (Vector3 1 1 1)

mirror :: Mirror
mirror = Mirror

spheres :: BHV Sphere
spheres = create_bhv vec
  where
    vec = V.generate 5 tosphere
    tosphere idx =
      Sphere (Vector3 x y z) 0.5
      where
        x = (fromIntegral idx) * 1.1 - 3
        y = 3
        z = (fromIntegral idx) * 1.1 + 4

main :: IO ()
main = do
  image <- render scene setting
  writePng "example3.png" image
