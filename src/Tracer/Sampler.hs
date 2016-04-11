module Tracer.Sampler where

import Tracer.Base
import Tracer.Vec
import Tracer.Scene
import Control.Monad.Random
import Control.Monad.Reader

-- it will generate an sample from a uniform distribution
-- sample correctly
sample2d :: (Fractional a) => Int -> ((Scalar, Scalar) -> SceneM a) -> SceneM a
sample2d n func = do
  result <- sum <$> (sequence . (take n) . repeat $ sample2dOnce func)
  return (result / (fromRational $ fromIntegral n))

sample2dOnce :: ((Scalar, Scalar) -> SceneM a) -> SceneM a
sample2dOnce func = do
  rx <- lift $ getRandomR (0, 1)
  ry <- lift $ getRandomR (0, 1)
  func (rx, ry)

-- uniformally sample the point, range from the rigid square
uniformSample :: (Int, Int) -> SceneM Color
uniformSample (x, y) = do
  raynumber <- asks $ _maxraysample . snd
  height <- asks $ fromIntegral . _height . snd
  width <- asks $ fromIntegral . _width . snd
  let fx = fromIntegral x
      fy = fromIntegral y
      mapping (rx, ry) = ((rx+fx)/width, (ry+fy)/height)
  sample2d raynumber (raytrace . mapping)
