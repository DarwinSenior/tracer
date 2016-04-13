module Tracer.Sampler where

import           Tracer.Base hiding (SampleM)
import           Tracer.Vec
import           Tracer.Scene
import           Control.Monad.Random
import           Control.Monad.Reader
import qualified System.Random.Mersenne.Pure64 as R
import           Control.Monad.State.Strict
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as S

type SampleM = State R.PureMT

randR :: SampleM Scalar
randR = do
  mt <- get
  let (w, mt') = R.randomDouble mt
  put mt'
  return (realToFrac w)

randR2 :: SampleM (Scalar, Scalar)
randR2 = do
  x <- randR
  y <- randR
  return (x, y)

sN :: Int -> (V.Vector (Scalar, Scalar))
sN n = V.generate (n*n) g
  where
    rn = fromIntegral (n)
    g x = ((fromIntegral $ x `rem` n) / rn, (fromIntegral $ x `quot` n) / rn)

-- it will generate an sample from a uniform distribution sample correctly
sample2d :: (Fractional a) => Int -> ((Scalar, Scalar) -> SceneM a) -> SceneM a
sample2d n func = do
  -- result <- sum <$> (sequence . (take n) . repeat $ sample2dOnce func)
  result <- sum <$> (mapM func (sN n))
  return (result / (fromRational $ fromIntegral (n*n)))

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
      mapping (rx, ry) = ((rx + fx) / width, (ry + fy) / height)
  sample2d raynumber (raytrace . mapping)
