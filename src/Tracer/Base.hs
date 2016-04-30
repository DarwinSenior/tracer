{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Tracer.Base (module Tracer.Base, module Debug.Trace) where

import           Data.Vector.V3
import           Control.Monad.Reader
import           Control.Monad.Random
import           Data.Vector.Class
import           Debug.Trace

type Dir = Vector3

type Pos = Vector3

type Color = Vector3

-- Ray origin direction
data Ray = Ray !Pos !Dir deriving Show

-- Camera pos up front
data Camera = Camera !Pos !Dir !Dir deriving Show

-- Intersection distance norm position
data Intersection = Intersection !Scalar Dir Pos deriving Show

-- scatter ray intersection material
data Scatter = forall mat. (Material mat) => Scatter Ray Intersection mat

-- object geometry material
data Object = forall mat geo. (Geometry geo, Material mat) => Object geo mat

type SampleM = Rand StdGen

type SceneM = ReaderT (Scene, Setting) SampleM

type Lights = [Light']

data Light' = forall l. (Light l) => Light' l

data Scene = forall sc. (Screen sc) =>
                                        Scene
                                          { _cam :: Camera
                                          , _screen :: sc
                                          , _lights :: Lights
                                          , _objects :: [Object]
                                          }

data Setting =
       Setting
         { _maxraydepth :: !Int
         , _height :: !Int
         , _width :: !Int
         , _maxraysample :: !Int
         , _use_shadow :: !Bool
         , _maxlightsample :: !Int
         }

-- a geometry is able to intersect ray
class Geometry g where
  intersect :: g -> Ray -> Intersection

-- a material has correpsonding brdf which specifies the relationship of mat and for ray tracing
-- more than the first degree, it will also scatter across the whole range
class Material mat where
  -- brdf of Material -> input direction -> surface normal -> intersection position -> outputdir -> Color where out is towards
  -- light
  brdf :: mat -> Dir -> Dir -> Dir -> Pos -> Color
  scatter :: mat -> Intersection -> Ray -> SampleM [Ray]
  is_shadowable :: mat -> Bool
  is_shadowable _ = True

-- for a light it should determine how much light would recive given the scene and the intersection,
-- for this to succeed, we need to use scattering
class Light l where
  -- shade :: Scatter -> l -> SceneM Color
  shade :: l -> Scatter -> SceneM Color

-- this code is used because the compiler is not advanced enough to pass a multi-class map
shade' :: Scatter -> Light' -> SceneM Color
shade' scattered light' =
  case light' of
    Light' l -> shade l scattered

-- a scene would also happen
class Screen sc where
  shootRay :: sc -> Camera -> (Scalar, Scalar) -> SampleM [Ray]

class Texture tx where
  texture i: tx -> Pos -> Color
