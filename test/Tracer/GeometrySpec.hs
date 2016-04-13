module Tracer.GeometrySpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Control.Monad
import           Debug.Trace

import           Tracer.Base
import           Tracer.Vec
import           Tracer.Geometry.Sphere
import           Tracer.Intersection
import           Tracer.Ray

instance Arbitrary Vector3 where
  arbitrary = Vector3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Transformation where
  arbitrary = do
    v1 <- arbitrary
    let m1 = identity `transM` (v1 + 1)
    s1 <- arbitrary
    let m2 = m1 `scaleM` (s1 + 1)
    s2 <- arbitrary
    let m3 = m2 `rotateXYM` ((s2 + 1) / s2)
    return m2

spec :: Spec
spec = do
  sphereSpec
  transformSpec

sphereSpec = do
  describe "sphere intersect" $ do
    it "should intersect" $ do
      let sphere = Sphere (Vector3 0 0 5) 2
          ray = Ray (Vector3 0 0 0) (Vector3 0 0 1)
          Intersection dist norm _ = sphere `intersect` ray
      dist `shouldBe` 3
      norm `shouldBe` (Vector3 0 0 (-1))

shouldBeV :: Pos -> Pos -> IO ()
shouldBeV a b = do
  when (vmag (a - b) / vmag a > 1.0e-4) (traceM $ show a ++ show b)
  shouldSatisfy (a - b) (\x -> (vmag a) == 0 || (vmag x) / (vmag a) < 1.0e-4)

transformSpec = do
  describe "transformation should work correctly" $ do
    it "should have det 1 for identity" $ do
      (1 `transs` identity) `shouldBe` 1
    it "should have identity greate" $ property $
      \v -> (1 `transs` (identity `transM` v)) `shouldBe` 1
    it "translate means translate" $ property $
      \v -> v `shouldBe` ((tovec 0) `transp` (identity `transM` v))
    it "translate direction means nothing" $ property $
      \d -> tovec 0 `shouldBe` ((-d) `transp` (identity `transM` d))
    it "should be able to translate back" $ property $
      \m v -> (((v `transp` (inv m)) `transp` m) `shouldBeV` ((v `transp` m) `transp` (inv m)))
    it "should translate back dir" $ property $
      \m v -> (((v `transd` (inv m)) `transd` m) `shouldBeV` ((v `transd` m) `transd` (inv m)))
