module Test.Ctl.Serialization (suite) where

import Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Serialization.Lib (publicKey_fromBytes)
import Cardano.Types (PublicKey, Transaction, TransactionHash)
import Cardano.Types.BigNum (fromString, one) as BN
import Cardano.Types.PlutusData as PD
import Cardano.Types.PublicKey as PublicKey
import Contract.Keys (publicKeyFromBech32)
import Ctl.Internal.Helpers (liftM)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Data.Maybe (Maybe, isJust, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import JS.BigInt as BigInt
import Mote (group, test)
import Test.Ctl.Fixtures
  ( txBinaryFixture1
  , txBinaryFixture2
  , txBinaryFixture3
  , txBinaryFixture4
  , txBinaryFixture5
  , txBinaryFixture6
  , txFixture1
  , txFixture2
  , txFixture3
  , txFixture4
  , txFixture5
  , txFixture6
  , txOutputBinaryFixture1
  , txOutputFixture1
  )
import Test.Ctl.Utils (errMaybe)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "cardano-serialization-lib bindings" $ do
    group "conversion between types" $ do
      test "PublicKey serialization" do
        let
          pkStr =
            "ed25519_pk1p9sf9wz3t46u9ghht44203gerxt82kzqaqw74fqrmwjmdy8sjxmqknzq8j"
          mPk = publicKeyFromBech32 pkStr

        pk <- liftM
          (error $ "Failed to create PubKey from bech32string: " <> pkStr)
          mPk

        let
          pkBytes = PublicKey.toRawBytes pk
          (pk'' :: Maybe PublicKey) = wrap <$> toMaybe
            ( publicKey_fromBytes
                $ unwrap pkBytes
            )

        pk'' `shouldSatisfy` isJust
      test "newTransactionHash" do
        let
          txString =
            "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
          txBytes = hexToByteArrayUnsafe txString
        _txHash :: TransactionHash <- liftM (error $ "newTransactionHash")
          $ decodeCbor
          $ wrap txBytes
        pure unit
      test "PlutusData #1 - Constr" $ do
        let
          datum = PD.Constr BN.one
            [ PD.Integer (BigInt.fromInt 1)
            , PD.Integer (BigInt.fromInt 2)
            ]
        let _ = encodeCbor datum -- Checking no exception raised
        pure unit
      test "PlutusData #2 - Map" $ do
        let
          datum =
            PD.Map
              [ PD.Integer (BigInt.fromInt 1) /\ PD.Integer (BigInt.fromInt 2)
              , PD.Integer (BigInt.fromInt 3) /\ PD.Integer (BigInt.fromInt 4)
              ]
        let _ = encodeCbor datum -- Checking no exception raised
        pure unit
      test "PlutusData #3 - List" $ do
        let
          datum = PD.List
            [ PD.Integer (BigInt.fromInt 1), PD.Integer (BigInt.fromInt 2) ]
        let _ = encodeCbor datum -- Checking no exception raised
        pure unit
      test "PlutusData #4 - List" $ do
        let
          datum = PD.List
            [ PD.Integer (BigInt.fromInt 1), PD.Integer (BigInt.fromInt 2) ]
        let _ = encodeCbor datum -- Checking no exception raised
        pure unit
      test "PlutusData #5 - Bytes" $ do
        let datum = PD.Bytes $ hexToByteArrayUnsafe "00ff"
        let _ = encodeCbor datum -- Checking no exception raised
        pure unit
      test
        "PlutusData #6 - Integer 0 (regression to https://github.com/Plutonomicon/cardano-transaction-lib/issues/488 ?)"
        $ do
            let bytes = encodeCbor $ PD.Integer (BigInt.fromInt 0)
            byteArrayToHex (unwrap bytes) `shouldEqual` "00"
      test "TransactionOutput serialization" $ liftEffect do
        let bytes = unwrap $ encodeCbor txOutputFixture1
        byteArrayToHex bytes `shouldEqual` txOutputBinaryFixture1
      test "Transaction serialization #1" $
        serializeTX txFixture1 txBinaryFixture1
      test "Transaction serialization #2 - tokens" $
        serializeTX txFixture2 txBinaryFixture2
      test "Transaction serialization #3 - ada" $
        serializeTX txFixture3 txBinaryFixture3
      test "Transaction serialization #4 - ada + mint + certificates" $
        serializeTX txFixture4 txBinaryFixture4
      test "Transaction serialization #5 - plutus script" $
        serializeTX txFixture5 txBinaryFixture5
      test "Transaction serialization #6 - metadata" $
        serializeTX txFixture6 txBinaryFixture6
    group "Transaction Roundtrips" $ do
      test "Deserialization is inverse to serialization #1" $
        txSerializedRoundtrip txFixture1
      test "Deserialization is inverse to serialization #2" $
        txSerializedRoundtrip txFixture2
      test "Deserialization is inverse to serialization #3" $
        txSerializedRoundtrip txFixture3
      test "Deserialization is inverse to serialization #4" $
        txSerializedRoundtrip txFixture4
      test "Deserialization is inverse to serialization #5" $
        txSerializedRoundtrip txFixture5
      test "Deserialization is inverse to serialization #6" $
        txSerializedRoundtrip txFixture6
    group "BigNum tests" $ do
      test "BigNum ok" $ do
        let bn = "18446744073709551615"
        BN.fromString bn `shouldSatisfy` isJust
      test "BigNum overflow" $ do
        let bn' = "18446744073709551616"
        BN.fromString bn' `shouldSatisfy` isNothing
      test "BigNum negative" $ do
        let bnNeg = "-1"
        BN.fromString bnNeg `shouldSatisfy` isNothing

serializeTX :: Transaction -> String -> Aff Unit
serializeTX tx fixture =
  liftEffect $ do
    let bytes = unwrap $ encodeCbor tx
    byteArrayToHex bytes `shouldEqual` fixture

txSerializedRoundtrip :: Transaction -> Aff Unit
txSerializedRoundtrip tx = do
  let serialized = encodeCbor tx
  (deserialized :: Transaction) <- errMaybe "Cannot deserialize bytes" $
    decodeCbor serialized
  let expected = encodeCbor deserialized
  serialized `shouldEqual` expected
