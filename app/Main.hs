module Main where

import           Codec.Picture.Png
import qualified Data.Vector                 as V
import           Tracer.Base
import           Tracer.Camera
import           Tracer.Geometry.BHV
import           Tracer.Geometry.Circle
import           Tracer.Geometry.GeoInstance
import           Tracer.Geometry.Plane
import           Tracer.Geometry.Sphere
import           Tracer.Geometry.Triangle
import           Tracer.Light.AmbientLight
import           Tracer.Light.AreaLight
import           Tracer.Light.DirLight
import           Tracer.Light.PointLight
import           Tracer.Material.Emissive
import           Tracer.Material.Mirror
import           Tracer.Material.Phong
import           Tracer.Render
import           Tracer.Screen.Pinhole
import           Tracer.Texture.Checker
import           Tracer.Texture.MonoColor
import           Tracer.Vec

setting :: Setting
setting = Setting
  { _maxraydepth = 3
  , _height = 500
  , _width = 500
  , _maxraysample = 10
  , _use_shadow = True
  , _maxlightsample = 3
  }

pinhole :: Pinhole
pinhole = Pinhole 30 20 20

camera :: Camera
camera = create_camera (tovec 0) (Vector3 0 (-1) 0) (Vector3 0 0 1)

camera' :: Camera
camera' = (yaw (0.2)) . (roll (-0.2)) . (move (Vector3 (-4) (-4) 0))$ camera

circle :: Circle
circle = Circle (Vector3 0 (-8) 2) (normalize $ Vector3 0 (-1) 0) 2

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
plane = Plane (normalize $ Vector3 (0) (0) (-1)) (-30)

plane2 :: Plane
plane2 = Plane (Vector3 0 (-1) 0) (-10)

sphere :: Sphere
sphere = Sphere (Vector3 0 0 5) 2

triangle :: Triangle
triangle = create_triangle (Vector3 0 0 5) (Vector3 0 1 5) (Vector3 1 0 5)

geoinstance :: GeoInstance (BHV Sphere)
geoinstance = GeoInstance transformation spheres
  where transformation = identity `scaleM1` 2 `transM` (Vector3 (2) (-2) (-3)) `rotateXYM` 0.3 `transM` (Vector3 0 0 (5))

phong :: Phong (Checker Plane)
phong = Phong (Checker plane 1) 1 0 1

phong2 :: Phong MonoColor
phong2 = Phong (MonoColor $ Vector3 1 0 0) 0.9 0.1 1

phong3 :: Phong MonoColor
phong3 = Phong (MonoColor $ Vector3 1 1 0) 0.1 0.9 2

scene :: Scene
scene = Scene camera' pinhole [Light' arealight]
          [Object plane phong, Object plane2 phong2, Object spheres phong3, Object circle emmisive, Object geoinstance mirror]

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
  writePng "example4.png" image
