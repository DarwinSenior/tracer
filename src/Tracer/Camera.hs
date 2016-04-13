module Tracer.Camera (
    create_camera,
    Camera,
    move,
    roll,
    pitch,
    yaw,
    ) where

import           Tracer.Vec
import           Tracer.Base
import           Tracer.Geometry

-- Camera with Pos, Up dir, Front dir
instance Transformable Camera where
  trans (Camera pos up front) mat44 =
    Camera (pos `transd` mat44) (up `transp` mat44) (front `transp` mat44)

create_camera :: Pos -> Dir -> Dir -> Camera
create_camera pos up front = Camera pos (normalize up) (normalize front)

move :: Pos -> Camera -> Camera
move pos (Camera _ up front) = Camera pos up front

rotate :: Dir -> Dir -> Scalar -> Dir
rotate par orth v = normalize $ (cos v) *| par + (sin v) *| orth

pitch :: Scalar -> Camera -> Camera
pitch v (Camera pos up front) = (Camera pos new_up front)
  where
    right = up `cross` front
    new_right = rotate right up v
    new_up = front `cross` new_right

roll :: Scalar -> Camera -> Camera
roll v (Camera pos up front) = (Camera pos up new_front)
  where
    right = up `cross` front
    new_front = rotate front right v

yaw :: Scalar -> Camera -> Camera
yaw v (Camera pos up front) = (Camera pos new_up new_front)
  where
    right = up `cross` front
    new_up = rotate up front v
    new_front = right `cross` new_up
