module Tracer.Render where

import           Tracer.Base
import           Tracer.Vec
import           Tracer.Sampler
import           Codec.Picture.Types
import           Data.Convertible (convert)
import           Control.Monad.Reader
import           Control.Monad.Random
import qualified Control.Monad.Parallel as PM

color2pixel :: Color -> PixelRGB8
color2pixel v = PixelRGB8 (t p1) (t p2) (t p3)
  where
    Vector3 p1 p2 p3 = v
    t = (convert . (min 255) . (255 *))

-- render :: PrimMonad m => Scene -> Setting -> (Int, Int) -> m (Image PixelRGB8)
render :: Scene -> Setting -> IO (Image PixelRGB8)
render scene setting = do
  let height = _height setting
      width = _width setting
  img <- newMutableImage width height
  mapM_
    (\(x, y) -> do
       gen <- getStdGen
       let color = evalRand (runReaderT (uniformSample (x, y)) (scene, setting)) gen
       writePixel img x y (color2pixel color))
    [(x, y) | x <- [0 .. (width - 1)]
            , y <- [0 .. (height - 1)]]
  unsafeFreezeImage img
