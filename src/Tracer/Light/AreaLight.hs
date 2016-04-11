module Tracer.Light.AreaLight where

import           Tracer.Sampler
import           Tracer.Light
import           Tracer.Base
import           Tracer.Vec
import Tracer.Geometry
import           Tracer.Light.PointLight
import           Control.Monad.Reader

type Shape2DFunc = Scatter -> (Scalar, Scalar) -> SceneM Pos

data (Surfceable geo) => AreaLight geo = AreaLight geo Color

instance (Surfceable geo) => Light (AreaLight geo) where
  shade (AreaLight geo color) scattered = do
    sampleNum <- asks $ _maxlightsample . snd
    colours <- replicateM sampleNum $ do
                 pos <- sample2dOnce $ \x -> return $ surface geo x
                 shadelight (pointlightfunc $ PointLight pos color) scattered
    return $ (sum colours) / (tovec $ fromIntegral sampleNum)

