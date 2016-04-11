module Tracer.Vec (
    cross,
    dot,
    normalize,
    transp,
    transd,
    tovec,
    Vector3(..),
    module Data.Vector.Class
    ) where

import Tracer.Base (Pos, Dir)
import           Data.Vector.V3
import           Data.Vector.V4
import           Data.Vector.Transform.T4
import           Data.Vector.Class

tovec :: Scalar -> Vector3
tovec = vpromote

cross :: Pos -> Pos -> Pos
cross = vcross

dot :: Pos -> Pos -> Scalar
dot = vdot

normalize :: Dir -> Dir
normalize = vnormalise

trans' :: Scalar -> Dir -> Transform4 -> Dir
trans' v (Vector3 a b c) mat4 =
  let v4 = Vector4 a b c v
      (Vector4 a' b' c' _) = mat4 `transformP4` v4
  in (Vector3 a' b' c')

transp :: Dir -> Transform4 -> Dir
transp = trans' 1

transd :: Dir -> Transform4 -> Dir
transd = trans' 1
