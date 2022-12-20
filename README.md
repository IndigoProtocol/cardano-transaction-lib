# cardano-transaction-lib

This repository is a fork from https://github.com/Plutonomicon/cardano-transaction-lib and contains any necessary changes that provide either improvements that are currently not present in CTL or patches that are specific to Indigo which will most likely not be implemented in the upstream repository.

### Improvements (Potential PRs):
* Sharing wallets between Plutip tests: https://github.com/IndigoProtocol/cardano-transaction-lib/pull/1

### Patches (Breaking Changes):
* Using a CSL fork to serialize/deserialize unsorted maps: https://github.com/IndigoProtocol/cardano-transaction-lib/pull/2
* Having `epochSize` in `PlutipConfig` to prevent `CannotFindTimeInEraSummaries` error: https://github.com/Plutonomicon/cardano-transaction-lib/issues/1057