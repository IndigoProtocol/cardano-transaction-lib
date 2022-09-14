module CTL.Internal.QueryM.GetTxByHash where

import Prelude

import CTL.Internal.Base64 (toByteArray)
import CTL.Internal.Cardano.Types.Transaction (Transaction)
import CTL.Internal.Deserialization.Transaction (deserializeTransaction)
import CTL.Internal.QueryM (QueryM, mkDatumCacheRequest)
import CTL.Internal.QueryM.DatumCacheWsp as QueryM
import CTL.Internal.QueryM.Ogmios (TxHash)
import CTL.Internal.Types.CborBytes (CborBytes(CborBytes))
import Data.Either (hush)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

getTxByHash :: TxHash -> QueryM (Maybe Transaction)
getTxByHash txHash = do
  unwrap <$> mkDatumCacheRequest QueryM.getTxByHash _.getTxByHash txHash >>=
    maybe
      (pure Nothing)
      \txBase64 -> do
        let
          txBytes = CborBytes $ toByteArray txBase64
        maybe (liftEffect $ throw "Unable to decode transaction")
          (pure <<< Just)
          $ hush
          $ deserializeTransaction txBytes
