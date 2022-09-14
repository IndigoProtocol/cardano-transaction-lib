module Test.CTL.Integration (main, testPlan) where

import Prelude

import CTL.Contract.Config (testnetConfig)
import CTL.Contract.Monad (runContract, wrapContract)
import CTL.Internal.QueryM (runQueryM)
import CTL.Internal.QueryM.Config (testnetTraceQueryConfig)
import CTL.Internal.QueryM.EraSummaries (getEraSummaries)
import CTL.Internal.QueryM.SystemStart (getSystemStart)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Mote (skip)
import Mote.Monad (mapTest)
import Test.CTL.AffInterface as AffInterface
import Test.CTL.BalanceTx.Collateral as Collateral
import Test.CTL.Logging as Logging
import Test.CTL.PrivateKey as PrivateKey
import Test.CTL.TestM (TestPlanM)
import Test.CTL.Types.Interval as Types.Interval
import Test.CTL.Utils as Utils

-- Run with `spago test --main Test.CTL.Integration`
main :: Effect Unit
main = launchAff_ do
  Utils.interpret testPlan

-- Requires external services listed in README.md
testPlan :: TestPlanM (Aff Unit) Unit
testPlan = do
  mapTest runQueryM' AffInterface.suite
  -- These tests depend on assumptions about testnet history.
  -- We disabled them during transition from `testnet` to `preprod` networks.
  -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/945
  skip $ flip mapTest Types.Interval.suite \f -> runQueryM
    testnetTraceQueryConfig
    do
      eraSummaries <- getEraSummaries
      sysStart <- getSystemStart
      liftEffect $ f eraSummaries sysStart
  Collateral.suite
  PrivateKey.suite
  Logging.suite
  where
  runQueryM' =
    runContract (testnetConfig { suppressLogs = true }) <<< wrapContract
