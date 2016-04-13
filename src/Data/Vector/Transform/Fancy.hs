{- |
  Generically handle transforms, and things that are transformable.
-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Data.Vector.Transform.Fancy where

import           Data.Angle

import           Data.Vector.Class
import           Data.Vector.V1
import           Data.Vector.V2
import           Data.Vector.V3
import           Data.Vector.V4
import           Data.Vector.Fancy
import           Data.Vector.Transform.T1
import           Data.Vector.Transform.T2
import           Data.Vector.Transform.T3
import           Data.Vector.Transform.T4

-- | Class for transforms.
class HasSpace t => Transform t where
  -- | Transform a vector.
  transformP :: t -> Point t -> Point t

  -- | Build transform: translate by the given vector.
  translateT :: Point t -> t

  -- | Build transform: scale each coordinate axis according to the given vector.
  scaleT :: Point t -> t

  -- | Build transform: scale all axies uniformly.
  scaleT_ :: Scalar -> t

-- | Class for performing rotationes. (The rotations that exist vary with the number of spatial
-- dimensions available.)
class (Transform t) => Rotate t axis1 axis2 where
  -- | Build transform: rotate in the plane defined by the two axies.
  rotateT :: (Angle a) => axis1 -> axis2 -> a Scalar -> t

-- | Class for things that can be transformed. Includes instances for all the vector types.
class HasSpace x => Transformable x where
  -- | Apply a transformation.
  transform :: (Transform t, Point t ~ Point x) => t -> x -> x

instance HasSpace Transform1 where
  type Point Transform1 = Vector1

instance Transform Transform1 where
  transformP = transformP1
  translateT (Vector1 x) = Transform1 1 x
  scaleT (Vector1 x) = Transform1 x 0
  scaleT_ k = Transform1 k 0

instance HasSpace Transform2 where
  type Point Transform2 = Vector2

instance Transform Transform2 where
  transformP = transformP2
  translateT (Vector2 x y) = Transform2 1 0 x 0 1 y
  scaleT (Vector2 x y) = Transform2 x 0 0 0 y 0
  scaleT_ k = Transform2 k 0 0 0 k 0

instance HasSpace Transform3 where
  type Point Transform3 = Vector3

instance Transform Transform3 where
  transformP = transformP3
  translateT (Vector3 x y z) = Transform3 1 0 0 x 0 1 0 y 0 0 1 z
  scaleT (Vector3 x y z) = Transform3 x 0 0 0 0 y 0 0 0 0 z 0
  scaleT_ k = Transform3 k 0 0 0 0 k 0 0 0 0 k 0

instance HasSpace Transform4 where
  type Point Transform4 = Vector4

instance Transform Transform4 where
  transformP = transformP4
  translateT (Vector4 x y z w) = Transform4 1 0 0 0 x 0 1 0 0 y 0 0 1 0 z 0 0 0 1 w
  scaleT (Vector4 x y z w) = Transform4 x 0 0 0 0 0 y 0 0 0 0 0 z 0 0 0 0 0 w 0
  scaleT_ k = Transform4 k 0 0 0 0 0 k 0 0 0 0 0 k 0 0 0 0 0 k 0

instance Transformable Vector1 where
  transform = transformP

instance Transformable Vector2 where
  transform = transformP

instance Transformable Vector3 where
  transform = transformP

instance Transformable Vector4 where
  transform = transformP

instance Rotate Transform2 AxisX AxisY where
  rotateT _ _ a =
    let s = sine a
        c = cosine a
        s' = negate s
    in Transform2 c s' 0 s c 0

instance Rotate Transform3 AxisX AxisY where
  rotateT _ _ a =
    let s = sine a
        c = cosine a
        s' = negate s
    in Transform3 c s' 0 0 s c 0 0 0 0 1 0

instance Rotate Transform3 AxisX AxisZ where
  rotateT _ _ a =
    let s = sine a
        c = cosine a
        s' = negate s
    in Transform3 c 0 s' 0 0 1 0 0 s 0 c 0

instance Rotate Transform3 AxisY AxisZ where
  rotateT _ _ a =
    let s = sine a
        c = cosine a
        s' = negate s
    in Transform3 1 0 0 0 0 c s' 0 0 s c 0

instance Rotate Transform4 AxisX AxisY where
  rotateT _ _ a =
    let s = sine a
        c = cosine a
        s' = negate s
    in Transform4 c s' 0 0 0 s c 0 0 0 0 0 1 0 0 0 0 0 1 0

instance Rotate Transform4 AxisX AxisZ where
  rotateT _ _ a =
    let s = sine a
        c = cosine a
        s' = negate s
    in Transform4 c 0 s' 0 0 0 1 0 0 0 s 0 c 0 0 0 0 0 1 0

instance Rotate Transform4 AxisX AxisW where
  rotateT _ _ a =
    let s = sine a
        c = cosine a
        s' = negate s
    in Transform4 c 0 0 s' 0 0 1 0 0 0 0 0 1 0 0 s 0 0 c 0

instance Rotate Transform4 AxisY AxisZ where
  rotateT _ _ a =
    let s = sine a
        c = cosine a
        s' = negate s
    in Transform4 1 0 0 0 0 0 c s' 0 0 0 s c 0 0 0 0 0 1 0

instance Rotate Transform4 AxisY AxisW where
  rotateT _ _ a =
    let s = sine a
        c = cosine a
        s' = negate s
    in Transform4 1 0 0 0 0 0 c 0 s' 0 0 0 1 0 0 0 s 0 c 0

instance Rotate Transform4 AxisZ AxisW where
  rotateT _ _ a =
    let s = sine a
        c = cosine a
        s' = negate s
    in Transform4 1 0 0 0 0 0 1 0 0 0 0 0 c s' 0 0 0 s c 0
