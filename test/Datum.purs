module Test.Ctl.Datum
  ( main
  )
  where

import Contract.Prelude

import Contract.AssocMap (DatumMap(DatumMap), Map(Map))
import Ctl.Internal.FromData (fromData)
import Ctl.Internal.Serialization (serializeData)
import Ctl.Internal.ToData (toData)
import Ctl.Internal.Types.CborBytes (CborBytes, hexToCborBytesUnsafe)
import Data.BigInt (BigInt, fromInt)
import Test.Spec.Assertions (shouldEqual)

datum :: DatumMap BigInt String
datum = DatumMap $ Map $
  [ (fromInt 2 /\ "b")
  , (fromInt 4 /\ "d")
  , (fromInt 1 /\ "a")
  , (fromInt 3 /\ "c")
  ]

datumCBOR ∷ CborBytes
datumCBOR = hexToCborBytesUnsafe "a4024162044164014161034163"

main :: Effect Unit
main = do
   Just datumCBOR `shouldEqual` serializeData datum
   Just datum `shouldEqual` (fromData $ toData datum)