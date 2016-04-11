module Tracer.GeometrySpec (spec) where

import           Test.Hspec

import           Tracer.Base
import           Tracer.Vec
import           Tracer.Geometry.Sphere
import           Tracer.Intersection
import           Tracer.Ray

spec :: Spec
spec = do
  sphereSpec

sphereSpec = do
  describe "sphere intersect" $ do
    it "should intersect" $ do
      let sphere = Sphere (Vector3 0 0 5) 2
          ray = Ray (Vector3 0 0 0) (Vector3 0 0 1)
          Intersection dist norm _ = sphere `intersect` ray
      dist `shouldBe` 3
      norm `shouldBe` (Vector3 0 0 (-1))
