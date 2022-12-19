module Test.Ctl.Datum
  ( main
  , suite
  ) where

import Contract.Prelude

import Contract.AssocMap (DatumMap(DatumMap), Map(Map))
import Ctl.Internal.Deserialization.PlutusData (deserializeData)
import Ctl.Internal.FromData (fromData)
import Ctl.Internal.Serialization (serializeData)
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Ctl.Internal.ToData (toData)
import Ctl.Internal.Types.CborBytes (CborBytes, hexToCborBytesUnsafe)
import Data.BigInt (BigInt, fromInt)
import Effect.Aff (launchAff_)
import Mote (group, test)
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

-- Run with `spago test --main Test.Ctl.Datum`
main :: Effect Unit
main = launchAff_ do
  interpret suite

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Unsorted Datum Map" do
    test "To Data <=> From Data" do
      Just datum `shouldEqual` (fromData $ toData datum)
    test "Serialize Datum" do
      Just datumCBOR `shouldEqual` serializeData datum
    test "Deserialize Datum CBOR" do
      Just datum `shouldEqual` deserializeData datumCBOR
