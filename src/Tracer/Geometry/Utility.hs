module Tracer.Geometry.Utility where

import           Tracer.Vec
import           Tracer.Geometry.Triangle
import           Tracer.Geometry.BHV
import qualified Graphics.Formats.STL as STL
import qualified Data.Vector as V

stl2bhv :: STL.STL -> BHV Triangle
stl2bhv (STL.STL _ triangles) =
  create_bhv . V.fromList . map (getriangle) $ triangles
  where
    getriangle (STL.Triangle normal verities) =
      let tv (x, y, z) = Vector3 x y z
          (p1, p2, p3) = verities
      in case normal of
        Nothing -> create_triangle (tv p1) (tv p2) (tv p3)
        Just n  -> Triangle (tv p1) (tv p2) (tv p3) (tv n)
