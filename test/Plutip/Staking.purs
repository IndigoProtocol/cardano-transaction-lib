module Test.Ctl.Plutip.Staking
  ( suite
  ) where

import Prelude

import Contract.Address
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  , PubKeyHash(PubKeyHash)
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  )
import Contract.Credential (Credential(ScriptCredential))
import Contract.Hashing (plutusScriptStakeValidatorHash, publicKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.Prelude (liftM)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( PlutusScriptStakeValidator
  , StakeValidatorHash
  , ValidatorHash
  , validatorHash
  )
import Contract.Staking
  ( getPoolIds
  , getPoolParameters
  , getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  )
import Contract.Test.Plutip (runPlutipContract, withStakeKey)
import Contract.Time (getCurrentEpoch)
import Contract.Transaction
  ( Epoch
  , PoolPubKeyHash(PoolPubKeyHash)
  , balanceTx
  , signTransaction
  )
import Contract.TxConstraints
  ( DatumPresence(DatumWitness)
  , mustDelegateStakePlutusScript
  , mustDelegateStakePubKey
  , mustDeregisterStakePlutusScript
  , mustDeregisterStakePubKey
  , mustPayToScriptAddress
  , mustRegisterPool
  , mustRegisterStakePubKey
  , mustRegisterStakeScript
  , mustRetirePool
  , mustWithdrawStakePlutusScript
  , mustWithdrawStakePubKey
  )
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (withKeyWallet)
import Contract.Wallet.Key (keyWalletPrivateStakeKey, publicKeyFromPrivateKey)
import Control.Monad.Reader (asks)
import Ctl.Examples.AlwaysSucceeds (alwaysSucceedsScript)
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Serialization.Address (keyHashCredential, rewardAddress)
import Ctl.Internal.Types.BigNum as BigNum
import Data.Array (head)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Foldable (for_)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Seconds(Seconds))
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Plutip.Common (config, privateStakeKey)
import Test.Ctl.Plutip.Utils (submitAndLog)
import Test.Ctl.TestM (TestPlanM)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  let
    clusterConfig = config.clusterConfig
      { slotLength = Seconds 0.1
      , epochSize = UInt.fromInt 10
      }
    config' = config
      { clusterConfig = clusterConfig
      }

  group "Staking" do
    group "Register / Deregister stake key" do
      test "PubKey" do
        let
          distribution = withStakeKey privateStakeKey
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 2_000_000_000
            ]
        runPlutipContract config' distribution $ flip withKeyWallet do
          alicePkh /\ aliceStakePkh <- do
            Tuple <$> liftedM "Failed to get PKH" ownPaymentPubKeyHash <*>
              liftedM "Failed to get Stake PKH" ownStakePubKeyHash

          -- Register
          do
            let
              constraints = mustRegisterStakePubKey aliceStakePkh

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh

            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

          -- Deregister stake key
          do
            let
              constraints = mustDeregisterStakePubKey aliceStakePkh

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh

            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

      test "PlutusScript" do
        let
          distribution = withStakeKey privateStakeKey
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 2_000_000_000
            ]
        runPlutipContract config' distribution $ flip withKeyWallet do
          alicePkh /\ aliceStakePkh <- do
            Tuple <$> liftedM "Failed to get PKH" ownPaymentPubKeyHash <*>
              liftedM "Failed to get Stake PKH" ownStakePubKeyHash
          validator <- alwaysSucceedsScript <#> unwrap >>> wrap
          let validatorHash = plutusScriptStakeValidatorHash validator

          -- Register
          do
            let
              constraints = mustRegisterStakeScript validatorHash

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh

            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

          -- Deregister stake key
          do
            let
              constraints = mustDeregisterStakePlutusScript validator
                unitRedeemer

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh

            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

    test "Pool registration & retirement" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config' distribution \alice -> withKeyWallet alice do
        alicePkh /\ aliceStakePkh <- Tuple
          <$> liftedM "Failed to get PKH" ownPaymentPubKeyHash
          <*> liftedM "Failed to get Stake PKH" ownStakePubKeyHash

        -- Register stake key
        do
          let
            constraints = mustRegisterStakePubKey aliceStakePkh

            lookups :: Lookups.ScriptLookups Void
            lookups =
              Lookups.ownPaymentPubKeyHash alicePkh <>
                Lookups.ownStakePubKeyHash aliceStakePkh

          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

        privateStakeKey <- liftM (error "Failed to get private stake key") $
          keyWalletPrivateStakeKey alice
        networkId <- asks $ unwrap >>> _.config >>> _.networkId
        let
          poolOperator = PoolPubKeyHash $ publicKeyHash $
            publicKeyFromPrivateKey (unwrap privateStakeKey)

        -- Register pool
        do
          let
            rewardAccount =
              rewardAddress
                { network: networkId
                , paymentCred: keyHashCredential $ unwrap $ unwrap aliceStakePkh
                }
            vrfKeyHash = unsafePartial $ fromJust $ fromBytes
              $ unsafePartial
              $ fromJust
              $ hexToByteArray
                  "fbf6d41985670b9041c5bf362b5262cf34add5d265975de176d613ca05f37096"
            poolParams =
              { operator: poolOperator
              , vrfKeyhash: vrfKeyHash -- needed to prove that the pool won the lottery
              , pledge: unsafePartial $ fromJust $ BigNum.fromBigInt $
                  BigInt.fromInt 1
              , cost: unsafePartial $ fromJust $ BigNum.fromBigInt $
                  BigInt.fromInt 1
              , margin:
                  { numerator: unsafePartial $ fromJust $ BigNum.fromBigInt $
                      BigInt.fromInt 1
                  , denominator: unsafePartial $ fromJust $ BigNum.fromBigInt $
                      BigInt.fromInt 1
                  }
              , rewardAccount
              , poolOwners:
                  [ PaymentPubKeyHash $ PubKeyHash $ publicKeyHash $
                      publicKeyFromPrivateKey
                        (unwrap privateStakeKey)
                  ]
              , relays: []
              , poolMetadata: Nothing
              }

            constraints = mustRegisterPool poolParams

            lookups :: Lookups.ScriptLookups Void
            lookups =
              Lookups.ownPaymentPubKeyHash alicePkh <>
                Lookups.ownStakePubKeyHash aliceStakePkh

          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

        -- List pools: the pool must appear in the list
        do
          pools <- getPoolIds
          logInfo' "Pool IDs:"
          logInfo' $ show pools
          for_ pools \poolId -> do
            logInfo' "Pool parameters"
            logInfo' <<< show =<< getPoolParameters poolId
          pools `shouldSatisfy` Array.elem poolOperator

        currentEpoch <- getCurrentEpoch
        let
          -- NOTE: this is a source of flaky-ness
          -- (there's no guarantee that the tx will pass before the specified epoch).
          -- You will get something like this error if it's not the case:
          -- Error: `submit` call failed. Error from Ogmios: [{"wrongRetirementEpoch":{"currentEpoch":114,"firstUnreachableEpoch":1000114,"requestedEpoch":95}}]
          retirementEpoch :: Epoch
          retirementEpoch = wrap (unwrap currentEpoch + UInt.fromInt 5)

        -- Retire pool
        do
          let
            constraints = mustRetirePool poolOperator retirementEpoch

            lookups :: Lookups.ScriptLookups Void
            lookups =
              Lookups.ownPaymentPubKeyHash alicePkh <>
                Lookups.ownStakePubKeyHash aliceStakePkh

          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

        let
          waitEpoch :: forall (r :: Row Type). Epoch -> Contract r Epoch
          waitEpoch epoch = do
            epochNow <- getCurrentEpoch
            if unwrap epochNow >= unwrap epoch then pure epochNow
            else do
              liftAff $ delay $ wrap 1000.0
              waitEpoch epoch

        void $ waitEpoch retirementEpoch

        -- List pools: the pool must not appear in the list
        do
          pools <- getPoolIds
          logInfo' "Pool IDs:"
          logInfo' $ show pools
          for_ pools \poolId -> do
            logInfo' "Pool parameters"
            logInfo' <<< show =<< getPoolParameters poolId
          pools `shouldSatisfy` (not <<< Array.elem poolOperator)

    test "Stake scripts: register, delegate" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigInt.fromInt 1_000_000_000 * BigInt.fromInt 1_000
          , BigInt.fromInt 2_000_000_000 * BigInt.fromInt 1_000
          ]
      runPlutipContract config' distribution \alice ->
        withKeyWallet alice do
          pure unit
          alicePkh /\ aliceStakePkh <- Tuple
            <$> liftedM "Failed to get PKH" ownPaymentPubKeyHash
            <*> liftedM "Failed to get Stake PKH" ownStakePubKeyHash
          (validator :: PlutusScriptStakeValidator) <- alwaysSucceedsScript <#>
            unwrap >>> wrap
          let
            (validatorHash :: ValidatorHash) = validatorHash $ wrap $ unwrap
              validator
          let
            (stakeValidatorHash :: StakeValidatorHash) = wrap $ unwrap
              validatorHash

          -- Lock funds on the stake script
          do
            let
              constraints =
                mustPayToScriptAddress (wrap $ unwrap validatorHash)
                  (ScriptCredential validatorHash)
                  unitDatum
                  DatumWitness
                  $ lovelaceValueOf
                  $ BigInt.fromInt 1_000_000_000 * BigInt.fromInt 100

              lookups :: Lookups.ScriptLookups Void
              lookups = mempty

            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

          -- Register stake script
          do
            let
              constraints =
                mustRegisterStakeScript stakeValidatorHash

              lookups :: Lookups.ScriptLookups Void
              lookups = mempty

            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

          -- List pools
          poolId <- do
            pools <- getPoolIds
            logInfo' "Pool IDs:"
            logInfo' $ show pools
            for_ pools \poolId -> do
              logInfo' "Pool parameters"
              logInfo' <<< show =<< getPoolParameters poolId
            liftM (error "unable to get any pools") (pools Array.!! 2)

          -- Delegate
          do
            let
              constraints =
                mustDelegateStakePlutusScript validator unitRedeemer poolId

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh
            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

          -- Wait until rewards
          let
            -- No need for limit on number of retries, because we have a
            -- timeout for tests.
            waitUntilRewards = do
              mbDelegationsAndRewards <-
                getValidatorHashDelegationsAndRewards stakeValidatorHash
              (mbDelegationsAndRewards <#> _.delegate) `shouldEqual` Just
                (Just poolId)
              case mbDelegationsAndRewards of
                Just dels@{ rewards } | unwrap <$> rewards > Just zero ->
                  pure dels
                _ -> do
                  liftAff $ delay $ wrap 5000.0
                  waitUntilRewards

          { rewards: rewardsBefore } <- waitUntilRewards

          -- Withdraw
          do
            let
              constraints =
                mustWithdrawStakePlutusScript validator unitRedeemer

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh
            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

          -- Check rewards.
          -- Not going to deregister here, because the rewards are added too
          -- soon, and we can't deregister the stake key if there are rewards
          -- left.
          -- This will not happen in real life scenarios, because epoch are
          -- (usually) significantly longer.
          do
            { rewards: rewardsAfter } <- liftedM "Unable to get rewards" $
              getValidatorHashDelegationsAndRewards stakeValidatorHash
            rewardsAfter `shouldSatisfy` \after -> after < rewardsBefore

    test "PubKey: delegation to existing pool & withdrawals" do
      let
        distribution = withStakeKey privateStakeKey
          [ BigInt.fromInt 1_000_000_000 * BigInt.fromInt 1_000
          , BigInt.fromInt 2_000_000_000 * BigInt.fromInt 1_000
          ]
      runPlutipContract config' distribution \alice ->
        withKeyWallet alice do
          alicePkh /\ aliceStakePkh <- Tuple
            <$> liftedM "Failed to get PKH" ownPaymentPubKeyHash
            <*> liftedM "Failed to get Stake PKH" ownStakePubKeyHash

          -- Register stake key
          do
            let
              constraints = mustRegisterStakePubKey aliceStakePkh

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh
            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

          -- List pools
          poolId <- do
            pools <- getPoolIds
            logInfo' "Pool IDs:"
            logInfo' $ show pools
            for_ pools \poolId -> do
              logInfo' "Pool parameters"
              logInfo' <<< show =<< getPoolParameters poolId
            liftM (error "unable to get any pools") (head pools)

          -- Delegate
          do
            let
              constraints =
                mustDelegateStakePubKey aliceStakePkh poolId

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh
            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

          -- Wait until rewards
          let
            -- No need for limit on number of retries, because we have a
            -- timeout for tests.
            waitUntilRewards = do
              mbDelegationsAndRewards <-
                getPubKeyHashDelegationsAndRewards aliceStakePkh
              (mbDelegationsAndRewards <#> _.delegate) `shouldEqual` Just
                (Just poolId)
              case mbDelegationsAndRewards of
                Just dels@{ rewards } | unwrap <$> rewards > Just zero ->
                  pure dels
                _ -> do
                  liftAff $ delay $ wrap 5000.0
                  waitUntilRewards

          { rewards: rewardsBefore } <- waitUntilRewards

          -- Withdraw
          do
            let
              constraints =
                mustWithdrawStakePubKey aliceStakePkh

              lookups :: Lookups.ScriptLookups Void
              lookups =
                Lookups.ownPaymentPubKeyHash alicePkh <>
                  Lookups.ownStakePubKeyHash aliceStakePkh
            ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
            liftedE (balanceTx ubTx) >>= signTransaction >>= submitAndLog

          -- Check rewards.
          -- Not going to deregister here, because the rewards are added too
          -- soon, and we can't deregister the stake key if there are rewards
          -- left.
          -- This will not happen in real life scenarios, because epoch are
          -- (usually) significantly longer.
          do
            { rewards: rewardsAfter } <-
              liftedM "Unable to get rewards"
                $ getPubKeyHashDelegationsAndRewards aliceStakePkh
            rewardsAfter `shouldSatisfy` \after -> after < rewardsBefore
