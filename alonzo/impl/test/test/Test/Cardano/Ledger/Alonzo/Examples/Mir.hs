{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Alonzo.Examples.Mir
-- Description : MIR Cert Examples
--
-- Examples demonstrating validity of MIR Certs in the Alonzo Era
module Test.Cardano.Ledger.Alonzo.Examples.Mir
  ( mirExamples,
  )
where

import Cardano.Ledger.Alonzo.Rules.Deleg (DelegPredicateFailure (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Shelley.Spec.Ledger.API
  ( AccountState (..),
    Credential (..),
    InstantaneousRewards (..),
    MIRPot (..),
    MIRTarget (..),
  )
import Shelley.Spec.Ledger.Coin (Coin (..), DeltaCoin (..))
import Shelley.Spec.Ledger.Keys
  ( KeyRole (..),
    hashKey,
  )
import Test.Cardano.Ledger.Alonzo.Examples (testAlonzoMIR)
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import Test.Shelley.Spec.Ledger.Utils (mkKeyPair)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

alice :: Credential 'Staking TestCrypto
alice = (KeyHashObj . hashKey . snd) $ mkKeyPair (0, 0, 0, 0, 1)

aliceOnlyReward :: Integer -> Map (Credential 'Staking TestCrypto) Coin
aliceOnlyReward c = Map.fromList [(alice, Coin c)]

aliceOnlyDelta :: Integer -> Map (Credential 'Staking TestCrypto) DeltaCoin
aliceOnlyDelta c = Map.fromList [(alice, DeltaCoin c)]

bob :: Credential 'Staking TestCrypto
bob = (KeyHashObj . hashKey . snd) $ mkKeyPair (0, 0, 0, 0, 2)

bobOnlyReward :: Integer -> Map (Credential 'Staking TestCrypto) Coin
bobOnlyReward c = Map.fromList [(bob, Coin c)]

bobOnlyDelta :: Integer -> Map (Credential 'Staking TestCrypto) DeltaCoin
bobOnlyDelta c = Map.fromList [(bob, DeltaCoin c)]

mirExamples :: TestTree
mirExamples =
  testGroup
    "MIR cert examples"
    [ testCase "increment reserves too much" $
        testAlonzoMIR
          ReservesMIR
          (StakeAddressesMIR $ aliceOnlyDelta 1)
          (InstantaneousRewards (aliceOnlyReward 1) mempty mempty mempty)
          (AccountState {_reserves = Coin 1, _treasury = Coin 0})
          (Left [[InsufficientForInstantaneousRewardsDELEG ReservesMIR (Coin 2) (Coin 1)]]),
      testCase "increment treasury too much" $
        testAlonzoMIR
          TreasuryMIR
          (StakeAddressesMIR $ aliceOnlyDelta 1)
          (InstantaneousRewards mempty (aliceOnlyReward 1) mempty mempty)
          (AccountState {_reserves = Coin 0, _treasury = Coin 1})
          (Left [[InsufficientForInstantaneousRewardsDELEG TreasuryMIR (Coin 2) (Coin 1)]]),
      testCase "increment reserves too much with delta" $
        testAlonzoMIR
          ReservesMIR
          (StakeAddressesMIR $ aliceOnlyDelta 1)
          (InstantaneousRewards (aliceOnlyReward 1) mempty (DeltaCoin (-1)) (DeltaCoin 1))
          (AccountState {_reserves = Coin 2, _treasury = Coin 0})
          (Left [[InsufficientForInstantaneousRewardsDELEG ReservesMIR (Coin 2) (Coin 1)]]),
      testCase "increment treasury too much with delta" $
        testAlonzoMIR
          TreasuryMIR
          (StakeAddressesMIR $ aliceOnlyDelta 1)
          (InstantaneousRewards mempty (aliceOnlyReward 1) (DeltaCoin 1) (DeltaCoin (-1)))
          (AccountState {_reserves = Coin 0, _treasury = Coin 2})
          (Left [[InsufficientForInstantaneousRewardsDELEG TreasuryMIR (Coin 2) (Coin 1)]]),
      testCase "negative balance in reserves mapping" $
        testAlonzoMIR
          ReservesMIR
          (StakeAddressesMIR $ aliceOnlyDelta (-1))
          (InstantaneousRewards mempty mempty mempty mempty)
          (AccountState {_reserves = Coin 1, _treasury = Coin 0})
          (Left [[MIRProducesNegativeUpdate]]),
      testCase "negative balance in treasury mapping" $
        testAlonzoMIR
          TreasuryMIR
          (StakeAddressesMIR $ aliceOnlyDelta (-1))
          (InstantaneousRewards mempty mempty mempty mempty)
          (AccountState {_reserves = Coin 0, _treasury = Coin 1})
          (Left [[MIRProducesNegativeUpdate]]),
      testCase "transfer reserves to treasury" $
        testAlonzoMIR
          ReservesMIR
          (OtherPot (Coin 1))
          (InstantaneousRewards mempty mempty mempty mempty)
          (AccountState {_reserves = Coin 1, _treasury = Coin 0})
          (Right (InstantaneousRewards mempty mempty (DeltaCoin (-1)) (DeltaCoin 1))),
      testCase "transfer treasury to reserves" $
        testAlonzoMIR
          TreasuryMIR
          (OtherPot (Coin 1))
          (InstantaneousRewards mempty mempty mempty mempty)
          (AccountState {_reserves = Coin 0, _treasury = Coin 1})
          (Right (InstantaneousRewards mempty mempty (DeltaCoin 1) (DeltaCoin (-1)))),
      testCase "insufficient transfer reserves to treasury" $
        testAlonzoMIR
          ReservesMIR
          (OtherPot (Coin 1))
          (InstantaneousRewards (aliceOnlyReward 1) mempty (DeltaCoin (-1)) (DeltaCoin 1))
          (AccountState {_reserves = Coin 2, _treasury = Coin 0})
          (Left [[InsufficientForTransferDELEG ReservesMIR (Coin 1) (Coin 0)]]),
      testCase "insufficient transfer treasury to reserves" $
        testAlonzoMIR
          TreasuryMIR
          (OtherPot (Coin 1))
          (InstantaneousRewards mempty (aliceOnlyReward 1) (DeltaCoin 1) (DeltaCoin (-1)))
          (AccountState {_reserves = Coin 0, _treasury = Coin 2})
          (Left [[InsufficientForTransferDELEG TreasuryMIR (Coin 1) (Coin 0)]]),
      testCase "increment reserves mapping" $
        testAlonzoMIR
          ReservesMIR
          (StakeAddressesMIR $ (aliceOnlyDelta 1 `Map.union` bobOnlyDelta 1))
          (InstantaneousRewards (aliceOnlyReward 1) mempty mempty mempty)
          (AccountState {_reserves = Coin 3, _treasury = Coin 0})
          ( Right
              ( InstantaneousRewards
                  (aliceOnlyReward 2 `Map.union` bobOnlyReward 1)
                  mempty
                  mempty
                  mempty
              )
          ),
      testCase "increment treasury mapping" $
        testAlonzoMIR
          TreasuryMIR
          (StakeAddressesMIR $ (aliceOnlyDelta 1 `Map.union` bobOnlyDelta 1))
          (InstantaneousRewards mempty (aliceOnlyReward 1) mempty mempty)
          (AccountState {_reserves = Coin 0, _treasury = Coin 3})
          ( Right
              ( InstantaneousRewards
                  mempty
                  (aliceOnlyReward 2 `Map.union` bobOnlyReward 1)
                  mempty
                  mempty
              )
          )
    ]
