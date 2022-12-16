module Test.Ctl.Datum where

import Contract.Prelude

import Contract.AssocMap (DatumMap(DatumMap), Map(Map))
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Serialization (serializeData, toBytes)
import Ctl.Internal.Serialization.PlutusData (convertPlutusData)
import Ctl.Internal.ToData (toData)
import Data.BigInt (BigInt, fromInt)
import Data.Newtype (unwrap)
import Effect.Console (logShow)
import Test.Ctl.Utils (errMaybe)
import Test.Spec.Assertions (shouldEqual)
import Untagged.Union (asOneOf)

datum :: DatumMap BigInt String
datum = DatumMap $ Map $
  [ (fromInt 2 /\ "b")
  , (fromInt 4 /\ "d")
  , (fromInt 1 /\ "a")
  , (fromInt 3 /\ "c")
  ]

main :: Effect Unit
main = do
  -- let pd = convertPlutusData $ toData datum
  -- bytes <- errMaybe "Failed serialization" $ (toBytes <<< asOneOf) <$> pd
  -- let res = fromBytes bytes
  -- res `shouldEqual` pd
  logShow $ serializeData datum