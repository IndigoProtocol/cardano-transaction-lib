module Ctl.Internal.Deserialization.PlutusData
  ( convertPlutusData
  , deserializeData
  ) where

import Prelude

import Control.Alt ((<|>))
import Ctl.Internal.Deserialization.BigInt (convertBigInt)
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.FfiHelpers
  ( ContainerHelper
  , MaybeFfiHelper
  , containerHelper
  , maybeFfiHelper
  )
import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.Serialization.Types
  ( BigInt
  , ConstrPlutusData
  , PlutusData
  , PlutusDatumMap
  , PlutusList
  , PlutusMap
  )
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.BigNum (toBigInt) as BigNum
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.PlutusData
  ( PlutusData(Constr, Map, List, Integer, Bytes)
  ) as T
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))

convertPlutusData :: PlutusData -> Maybe T.PlutusData
convertPlutusData pd =
  convertPlutusConstr pd
    <|> convertMap pd
    <|> convertPlutusList pd
    <|> convertPlutusInteger pd
    <|> convertPlutusBytes pd

convertPlutusConstr :: PlutusData -> Maybe T.PlutusData
convertPlutusConstr pd = do
  constr <- _PlutusData_constr maybeFfiHelper pd
  data' <- traverse convertPlutusData
    $ _unpackPlutusList containerHelper
    $ _ConstrPlutusData_data constr
  alt <- BigNum.toBigInt $ _ConstrPlutusData_alternative constr
  pure $ T.Constr alt data'

-- With no good way of knowing the data types that are contained in a map defore
-- deserialization, we can assume that CSL's default map deserialization should
-- be used, unless the original bytes of the `PlutusData` is provided to CSL.
-- The key-value pairs should be returned unsorted by defalut, since CSL should
-- always be storing the original bytes while deserializing `PlutusData`. The 
-- output of `convertMap` should be `Map` instead of the ad hoc `DatumMap`,
-- since a CBOR map is not limited to a datum.
convertMap :: PlutusData -> Maybe T.PlutusData
convertMap pd = do
  case getOriginalBytes pd of
    Just (bytes) -> convertDatumMap bytes
    Nothing -> convertPlutusMap pd

convertPlutusMap :: PlutusData -> Maybe T.PlutusData
convertPlutusMap pd = do
  entries <- _PlutusData_map maybeFfiHelper pd >>=
    _unpackPlutusMap containerHelper Tuple >>> traverse
      \(k /\ v) -> do
        k' <- convertPlutusData k
        v' <- convertPlutusData v
        pure (k' /\ v')
  pure $ T.Map entries

convertDatumMap :: ByteArray -> Maybe T.PlutusData
convertDatumMap bytes = do
  entries <-
    (fromBytes bytes :: Maybe PlutusDatumMap) >>=
      _unpackPlutusDatumMap containerHelper Tuple >>> traverse
        \(k /\ v) -> do
          k' <- convertPlutusData k
          v' <- convertPlutusData v
          pure (k' /\ v')
  pure $ T.Map entries

convertPlutusList :: PlutusData -> Maybe T.PlutusData
convertPlutusList pd = T.List <$> do
  _PlutusData_list maybeFfiHelper pd >>=
    _unpackPlutusList containerHelper >>>
      traverse convertPlutusData

convertPlutusInteger :: PlutusData -> Maybe T.PlutusData
convertPlutusInteger pd = T.Integer <$> do
  _PlutusData_integer maybeFfiHelper pd >>= convertBigInt

convertPlutusBytes :: PlutusData -> Maybe T.PlutusData
convertPlutusBytes pd = T.Bytes <$> _PlutusData_bytes maybeFfiHelper pd

deserializeData :: forall (a :: Type). FromData a => CborBytes -> Maybe a
deserializeData = (fromData <=< convertPlutusData <=< fromBytes) <<< unwrap

getOriginalBytes :: PlutusData -> Maybe ByteArray
getOriginalBytes = _PlutusData_originalBytes maybeFfiHelper

foreign import _PlutusData_constr
  :: MaybeFfiHelper -> PlutusData -> Maybe ConstrPlutusData

foreign import _PlutusData_map
  :: MaybeFfiHelper -> PlutusData -> Maybe PlutusMap

foreign import _PlutusData_list
  :: MaybeFfiHelper -> PlutusData -> Maybe PlutusList

foreign import _PlutusData_integer
  :: MaybeFfiHelper -> PlutusData -> Maybe BigInt

foreign import _PlutusData_bytes
  :: MaybeFfiHelper -> PlutusData -> Maybe ByteArray

foreign import _unpackPlutusList
  :: ContainerHelper -> PlutusList -> Array PlutusData

foreign import _ConstrPlutusData_alternative :: ConstrPlutusData -> BigNum
foreign import _ConstrPlutusData_data :: ConstrPlutusData -> PlutusList
foreign import _unpackPlutusMap
  :: ContainerHelper
  -> (forall a b. a -> b -> Tuple a b)
  -> PlutusMap
  -> Array (PlutusData /\ PlutusData)

foreign import _unpackPlutusDatumMap
  :: ContainerHelper
  -> (forall a b. a -> b -> Tuple a b)
  -> PlutusDatumMap
  -> Array (PlutusData /\ PlutusData)

foreign import _PlutusData_originalBytes
  :: MaybeFfiHelper -> PlutusData -> Maybe ByteArray
