module Tracer.ScreenSpec where

import           Test.Hspec
import           Tracer.Base
import           Tracer.Vec
import           Tracer.Camera

import qualified Tracer.Screen.Pinhole as P
import           Tracer.Ray

spec :: Spec
spec = do
  pinholeSpec

pinholeSpec = do
  describe "pinhole shoot" $ do
    it "should in the center" $ do
      let cam = Camera (tovec 0) (Vector3 0 (-1) 0) (Vector3 0 0 1)
          pinhole = P.Pinhole 2 2 2
          Ray o d = P.shoot pinhole cam (0.5, 0.5)
      o `shouldBe` Vector3 0 0 0
      d `shouldBe` Vector3 0 0 1
