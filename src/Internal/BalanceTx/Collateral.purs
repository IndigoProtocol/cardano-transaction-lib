module CTL.Internal.BalanceTx.Collateral
  ( addTxCollateral
  , addTxCollateralReturn
  , module X
  ) where

import Prelude

import CTL.Internal.BalanceTx.Collateral.Select (minRequiredCollateral)
import CTL.Internal.BalanceTx.Collateral.Select (minRequiredCollateral) as X
import CTL.Internal.BalanceTx.Error
  ( BalanceTxError(CollateralReturnError, CollateralReturnMinAdaValueCalcError)
  )
import CTL.Internal.BalanceTx.Types
  ( BalanceTxM
  )
import CTL.Internal.BalanceTx.UtxoMinAda (utxoMinAdaValue)
import CTL.Internal.Cardano.Types.Transaction
  ( Transaction
  , TransactionOutput
  , _body
  , _collateral
  , _collateralReturn
  , _totalCollateral
  )
import CTL.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
import CTL.Internal.Cardano.Types.Value (Coin, NonAdaAsset)
import CTL.Internal.Cardano.Types.Value (getNonAdaAsset, mkValue, valueToCoin') as Value
import CTL.Internal.Serialization.Address (Address)
import CTL.Internal.Types.BigNum (maxValue, toBigIntUnsafe) as BigNum
import CTL.Internal.Types.OutputDatum (OutputDatum(NoOutputDatum))
import Control.Monad.Except.Trans (ExceptT(ExceptT), except)
import Control.Monad.Reader.Class (asks)
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), note)
import Data.Foldable (foldMap, foldl)
import Data.Lens.Setter ((?~))
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Ord.Max (Max(Max))
import Effect.Class (liftEffect)

addTxCollateral :: Array TransactionUnspentOutput -> Transaction -> Transaction
addTxCollateral collateral transaction =
  transaction # _body <<< _collateral ?~
    map (_.input <<< unwrap) collateral

--------------------------------------------------------------------------------
-- Collateral Return, Total Collateral
--------------------------------------------------------------------------------

-- | Sets `collateral return` and `total collateral` fields of the transaction.
-- | In the special case with an Ada-only collateral that is less than or equal
-- | to `minRequiredCollateral`, returns unmodified transaction (see NOTE).
-- |
-- | NOTE: Collateral cannot be less than `minRequiredCollateral` when
-- | selected using `selectCollateral` function in this module.
addTxCollateralReturn
  :: Array TransactionUnspentOutput
  -> Transaction
  -> Address
  -> BalanceTxM Transaction
addTxCollateralReturn collateral transaction ownAddress =
  let
    collAdaValue :: BigInt
    collAdaValue = foldl adaValue' zero collateral

    collNonAdaAsset :: NonAdaAsset
    collNonAdaAsset = foldMap nonAdaAsset collateral
  in
    case collAdaValue <= minRequiredCollateral && collNonAdaAsset == mempty of
      true ->
        pure transaction
      false ->
        setTxCollateralReturn collAdaValue collNonAdaAsset
  where
  setTxCollateralReturn
    :: BigInt
    -> NonAdaAsset
    -> BalanceTxM Transaction
  setTxCollateralReturn collAdaValue collNonAdaAsset = do
    let
      maxBigNumAdaValue :: Coin
      maxBigNumAdaValue = wrap (BigNum.toBigIntUnsafe BigNum.maxValue)

      collReturnOutputRec =
        { address: ownAddress
        , amount: Value.mkValue maxBigNumAdaValue collNonAdaAsset
        , datum: NoOutputDatum
        , scriptRef: Nothing
        }

    coinsPerUtxoUnit <-
      asks (_.runtime >>> _.pparams) <#> unwrap >>> _.coinsPerUtxoUnit

    -- Calculate the required min ada value for the collateral return output:
    minAdaValue <-
      ExceptT $
        liftEffect (utxoMinAdaValue coinsPerUtxoUnit (wrap collReturnOutputRec))
          <#> note CollateralReturnMinAdaValueCalcError

    let
      -- Determine the actual ada value of the collateral return output:
      collReturnAda :: BigInt
      collReturnAda = unwrap $
        Max (collAdaValue - minRequiredCollateral) <> Max minAdaValue

      -- Build the final collateral return output:
      collReturnOutput :: TransactionOutput
      collReturnOutput = wrap $
        collReturnOutputRec
          { amount = Value.mkValue (wrap collReturnAda) collNonAdaAsset }

      totalCollateral :: BigInt
      totalCollateral = collAdaValue - collReturnAda

    except $
      case totalCollateral > zero of
        true ->
          -- Set collateral return and total collateral:
          Right $
            transaction # _body <<< _collateralReturn ?~ collReturnOutput
              # _body <<< _totalCollateral ?~ wrap totalCollateral
        false ->
          Left $ CollateralReturnError
            "Negative totalCollateral after covering min-utxo-ada requirement."

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

adaValue :: TransactionUnspentOutput -> BigInt
adaValue =
  Value.valueToCoin' <<< _.amount <<< unwrap <<< _.output <<< unwrap

adaValue' :: BigInt -> TransactionUnspentOutput -> BigInt
adaValue' init = add init <<< adaValue

nonAdaAsset :: TransactionUnspentOutput -> NonAdaAsset
nonAdaAsset =
  Value.getNonAdaAsset <<< _.amount <<< unwrap <<< _.output <<< unwrap
