module Ctl.Internal.Deserialization.PlutusData
  ( convertPlutusData
  , deserializeData
  ) where

import Prelude

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
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.CborBytes (CborBytes(CborBytes))
import Ctl.Internal.Types.PlutusData
  ( PlutusData(Constr, Map, List, Integer, Bytes)
  ) as T
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)

type ConvertPlutusData =
  { constr :: ConstrPlutusData -> T.PlutusData
  , map :: PlutusData -> T.PlutusData
  , list :: PlutusList -> T.PlutusData
  , integer :: BigInt -> T.PlutusData
  , bytes :: ByteArray -> T.PlutusData
  }

convertPlutusData :: PlutusData -> T.PlutusData
convertPlutusData pd = _convertPlutusData
  { constr: convertPlutusConstr
  , map: convertMap
  , list: convertPlutusList
  , integer: convertPlutusInteger
  , bytes: convertPlutusBytes
  }
  pd

convertPlutusConstr :: ConstrPlutusData -> T.PlutusData
convertPlutusConstr constr = do
  let
    data' = convertPlutusData <$>
      _unpackPlutusList containerHelper (_ConstrPlutusData_data constr)
    alt = _ConstrPlutusData_alternative constr
  T.Constr alt data'

-- With no good way of knowing the data types that are contained in a map defore
-- deserialization, we can assume that CSL's default map deserialization should
-- be used, unless the original bytes of the `PlutusData` is provided to CSL.
-- The key-value pairs should be returned unsorted by defalut, since CSL should
-- always be storing the original bytes while deserializing `PlutusData`. The 
-- output of `convertMap` should be `Map` instead of the ad hoc `DatumMap`,
-- since a CBOR map is not limited to a datum.
convertMap :: PlutusData -> T.PlutusData
convertMap pd = do
  case getOriginalBytes pd of
    Just (bytes) -> unsafePartial $ fromJust $ convertDatumMap bytes
    Nothing -> unsafePartial $ fromJust $ convertPlutusMap pd

convertDatumMap :: ByteArray -> Maybe T.PlutusData
convertDatumMap bytes = do
  entries <-
    (fromBytes (CborBytes bytes) :: Maybe PlutusDatumMap) <#>
      _unpackPlutusDatumMap containerHelper Tuple >>> map
      \(k /\ v) -> (convertPlutusData k /\ convertPlutusData v)
  pure $ T.Map entries

convertPlutusMap :: PlutusData -> Maybe T.PlutusData
convertPlutusMap pd = do
  entries <- _PlutusData_map maybeFfiHelper pd <#>
    _unpackPlutusMap containerHelper Tuple >>> map
      \(k /\ v) -> (convertPlutusData k /\ convertPlutusData v)
  pure $ T.Map entries

convertPlutusList :: PlutusList -> T.PlutusData
convertPlutusList =
  _unpackPlutusList containerHelper >>> map (\d -> convertPlutusData d) >>>
    T.List

-- Unsafe fromJust here is correct, due to arbitrary sized integers
convertPlutusInteger :: BigInt -> T.PlutusData
convertPlutusInteger i = T.Integer $ unsafePartial $ fromJust $ convertBigInt i

convertPlutusBytes :: ByteArray -> T.PlutusData
convertPlutusBytes = T.Bytes

deserializeData :: forall (a :: Type). FromData a => CborBytes -> Maybe a
deserializeData = fromData <<< convertPlutusData <=< fromBytes

getOriginalBytes :: PlutusData -> Maybe ByteArray
getOriginalBytes = _PlutusData_originalBytes maybeFfiHelper

foreign import _PlutusData_map
  :: MaybeFfiHelper -> PlutusData -> Maybe PlutusMap
  
foreign import _PlutusData_originalBytes
  :: MaybeFfiHelper -> PlutusData -> Maybe ByteArray

foreign import _convertPlutusData
  :: ConvertPlutusData -> PlutusData -> T.PlutusData

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
