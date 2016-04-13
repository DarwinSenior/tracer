module Tracer.Vec (module Tracer.Vec, module Data.Vector.V3, module Data.Vector.Class) where

import           Tracer.Base (Pos, Dir)
import           Data.List
import           Data.Vector.V3
import           Data.Vector.Transform.T3
import           Data.Vector.Class
import           Data.Vector.Transform.Fancy
import           Data.Vector.Fancy
import           Data.Angle

-- transform matrix matrix_inverse
data Transformation = Transformation Transform3 Transform3
  deriving Eq

instance Show Transformation where
  show (Transformation t1 t2) =
    (show t1) ++ "\n" ++ (show t2)

instance Monoid Transformation where
  mempty = Transformation mempty mempty
  (Transformation x x') `mappend` (Transformation y y') = Transformation (x `mappend` y)
                                                            (y' `mappend` x')

identity :: Transformation
identity = Transformation mempty mempty

transfunc :: (a -> Transform3) -> (a -> a) -> (Transformation -> a -> Transformation)
transfunc transf invf (Transformation mat mat') v =
  Transformation (transf v `mappend` mat) (mat' `mappend` transf (invf v))

transM :: Transformation -> Vector3 -> Transformation
transM = transfunc translateT negate

scaleM :: Transformation -> Vector3 -> Transformation
scaleM = transfunc scaleT recip

scaleM1 :: Transformation -> Scalar -> Transformation
scaleM1 = transfunc scaleT_ recip

rotateXYM :: Transformation -> Scalar -> Transformation
rotateXYM = transfunc (\x -> rotateT AxisX AxisY (Radians x)) negate

rotateXZM :: Transformation -> Scalar -> Transformation
rotateXZM = transfunc (\x -> rotateT AxisX AxisZ (Radians x)) negate

rotateYZM :: Transformation -> Scalar -> Transformation
rotateYZM = transfunc (\x -> rotateT AxisY AxisZ (Radians x)) negate

transp2d :: Transform3 -> Transform3
transp2d (Transform3 a b c _ d e f _ g h i _) =
  Transform3 a b c 0 d e f 0 g h i 0

det' :: Transform3 -> Scalar
det' (Transform3 a b c _ d e f _ g h i _) =
  a * (e * i - f * h) - b * (f * g - d * i) + c * (d * h - e * g)

inv :: Transformation -> Transformation
inv (Transformation x x') = Transformation x' x

transs :: Scalar -> Transformation -> Scalar
transs s (Transformation t _) = s / (det' t)

transd :: Dir -> Transformation -> Dir
transd d (Transformation t _) = vnormalise $ (transp2d t) `transformP3` d

transp :: Pos -> Transformation -> Pos
transp p (Transformation t _) = t `transformP3` p

tovec :: Scalar -> Vector3
tovec = vpromote

cross :: Pos -> Pos -> Pos
cross = vcross

dot :: Pos -> Pos -> Scalar
dot = vdot

normalize :: Dir -> Dir
normalize = vnormalise
