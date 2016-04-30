module Tracer.MaterialSpec (spec) where

import           Test.Hspec
import           Tracer.Base
import           Tracer.Material.Phong
import           Tracer.Ray
import           Tracer.Texture.MonoColor
import           Tracer.Vec

spec :: Spec
spec = do
  phongSpec
  raySpec

phongSpec = do
  describe "phong material" $ do
    it "should produce diffusive color" $ do
      let phong = Phong (MonoColor $ tovec 1) 1 0 0
          indir = Vector3 0 0 1
          outdir = indir
          norm = indir
      brdf phong indir norm outdir (tovec 0) `shouldBe` (tovec 1)

raySpec = do
  describe "ray model" $ do
    it "the normal will reflect itself" $ do
      let x = tovec 1
          y = normalize $ Vector3 3 4 5
          z = reflect x y
      normalize (z + x) `shouldSatisfy` (< 1.0e-4) . vmag . (+ (-y))
