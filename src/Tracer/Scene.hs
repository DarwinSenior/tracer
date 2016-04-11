{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tracer.Scene where

import           Tracer.Base
import           Tracer.Vec
import           Tracer.Intersection
import           Tracer.Ray
import           Tracer.Material
import           Tracer.Material.Emissive
import           Control.Monad.State.Strict
import           Control.Monad.Reader

-- test if the ray will be shadowed by the scene for emmisive object, it should not be included
inshadow :: Ray -> Scalar -> SceneM Bool
inshadow ray dist = do
  objects <- asks $ _objects . fst
  return $
    case raytrace_objects (rayshift ray) (filter should_shadow objects) of
      Scatter _ (Intersection newdist _ _) _ -> newdist < dist
  where should_shadow (Object _ mat) = is_shadowable mat

raytrace_objects :: Ray -> [Object] -> Scatter
raytrace_objects ray =
  foldl reduce (Scatter ray farest_intersection BlackBody)
  where
    reduce scattered@(Scatter _ inter _) (Object geo' mat') =
      if inter' < inter
        then Scatter ray inter' mat'
        else scattered
      where
        inter' = geo' `intersect` ray

ray2scatter :: Ray -> SceneM Scatter
ray2scatter ray = do
  objects <- asks $ _objects . fst
  return $ raytrace_objects ray objects

traceRay :: Int -> Ray -> SceneM Color
traceRay depth ray = do
  scattered <- ray2scatter ray
  scene <- asks fst
  colors <- mapM (shade' scattered) (_lights scene)
  let color = sum colors
  max_depth <- asks $ _maxraydepth . snd
  if depth < max_depth
    then do
      newrays <- case scattered of
                   Scatter _ inter mat -> lift $ scatter mat inter ray
      colors' <- mapM (traceRay $ depth + 1) newrays
      return $ (sum colors') + color
    else return color

-- this is for debugging
shadewhite' :: Scatter -> Color
shadewhite' scattered =
  case scattered of
    Scatter _ inter _ ->
      if inter < farest_intersection
        then tovec 1
        else tovec 0

raytrace :: (Scalar, Scalar) -> SceneM Color
raytrace (rx, ry) = do
  cam <- asks $ _cam . fst
  scene <- asks fst
  rays <- case scene of
            Scene _ screen _ _ -> lift $ shootRay screen cam (rx, ry)
  colors <- mapM (traceRay 0) rays
  return $ sum colors
