{- |
  Various facilities for dealing with vectors, vector spaces and coordinate axies generically.
-}

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

module Data.Vector.Fancy where

import Data.Vector.Class
import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3
import Data.Vector.V4

-- * Vector spaces

{- |
  This class deals with any type that has a spatial dimensionallity.
  This includes coordinate transforms, bounding boxes, and so on.

  Null instances are provided for all the vector types. (E.g.,
  @Point Vector3 = Vector3@.)
-}
class (Vector (Point x)) => HasSpace x where
  -- | Give the appropriate kind of vector for this type.
  type Point x :: *

instance HasSpace Vector1 where
  type Point Vector1 = Vector1

instance HasSpace Vector2 where
  type Point Vector2 = Vector2

instance HasSpace Vector3 where
  type Point Vector3 = Vector3

instance HasSpace Vector4 where
  type Point Vector4 = Vector4



-- * Vector axies

-- | The X-axis (first axis).
data AxisX = AxisX deriving Show

-- | The Y-axis (second axis).
data AxisY = AxisY deriving Show

-- | The Z-axis (third axis).
data AxisZ = AxisZ deriving Show

-- | The W-axis (fourth axis).
data AxisW = AxisW deriving Show

-- | Class for generically reading/writing vector coordinates.
class VectorAxis vector axis where
  -- | Read from the specified coordinate axis.
  get_coord :: axis -> vector -> Scalar

  -- | Replace the existing value of the given coordinate axis.
  set_coord :: axis -> Scalar -> vector -> vector

instance VectorAxis Vector1 AxisX where
  get_coord _ = v1x
  set_coord _ x _ = Vector1 x

instance VectorAxis Vector2 AxisX where
  get_coord _ = v2x
  set_coord _ x v = v {v2x = x}

instance VectorAxis Vector3 AxisX where
  get_coord _ = v3x
  set_coord _ x v = v {v3x = x}

instance VectorAxis Vector4 AxisX where
  get_coord _ = v4x
  set_coord _ x v = v {v4x = x}

instance VectorAxis Vector2 AxisY where
  get_coord _ = v2y
  set_coord _ y v = v {v2y = y}

instance VectorAxis Vector3 AxisY where
  get_coord _ = v3y
  set_coord _ y v = v {v3y = y}

instance VectorAxis Vector4 AxisY where
  get_coord _ = v4y
  set_coord _ y v = v {v4y = y}

instance VectorAxis Vector3 AxisZ where
  get_coord _ = v3z
  set_coord _ z v = v {v3z = z}

instance VectorAxis Vector4 AxisZ where
  get_coord _ = v4z
  set_coord _ z v = v {v4z = z}

instance VectorAxis Vector4 AxisW where
  get_coord _ = v4w
  set_coord _ w v = v {v4w = w}



-- * Vector projection

{- |
  This class enables you to take a vector with N dimensions and
  project it into an N+1 dimensional space (and also take the inverse
  projection to get back again).
-}
class (Vector v, Vector (ProjectTo v)) => Project v where
  -- | The next-largest vector type. (E.g., 'ProjectTo' 'Vector2' = 'Vector3'.)
  type ProjectTo v :: *

  -- | Reduce number of dimensions by one. (Return the dropped dimension as a @Scalar@.)
  orthographic_down ::  ProjectTo v -> (v, Scalar)

  -- | Increase number of dimensions by one. (Supply value for new dimension as a @Scalar@.)
  orthographic_up   :: (v, Scalar)  -> ProjectTo v

  -- | Perspective-project to N-1 dimensions. (Also return the distance from the camera as a @Scalar@.)
  perspective_down ::  ProjectTo v  -> (v, Scalar)

  -- | Inverse-perspective project into N+1 dimension. (Supply the distance from the camera as a @Scalar@.)
  perspective_up   :: (v, Scalar)   ->  ProjectTo v

instance Project Vector1 where
  type ProjectTo Vector1 = Vector2
  orthographic_down (Vector2 x  y) = (Vector1 x, y)
  orthographic_up   (Vector1 x, y) = (Vector2 x  y)

  perspective_down  (Vector2 x  y) = (Vector1 (x/y), y)
  perspective_up    (Vector1 x, y) = (Vector2 (x*y)  y)

instance Project Vector2 where
  type ProjectTo Vector2 = Vector3
  orthographic_down (Vector3 x y  z) = (Vector2 x y, z)
  orthographic_up   (Vector2 x y, z) = (Vector3 x y  z)

  perspective_down  (Vector3 x y  z) = (Vector2 (x/z) (y/z), z)
  perspective_up    (Vector2 x y, z) = (Vector3 (x*z) (y*z)  z)

instance Project Vector3 where
  type ProjectTo Vector3 = Vector4
  orthographic_down (Vector4 x y z  w) = (Vector3 x y z, w)
  orthographic_up   (Vector3 x y z, w) = (Vector4 x y z  w)

  perspective_down  (Vector4 x y z  w) = (Vector3 (x/w) (y/w) (z/w), w)
  perspective_up    (Vector3 x y z, w) = (Vector4 (x*w) (y*w) (z*w)  w)
