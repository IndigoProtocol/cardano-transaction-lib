/* global require exports BROWSER_RUNTIME */

var lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    lib = require('@ngua/cardano-serialization-lib-browser');
} else {
    lib = require('@ngua/cardano-serialization-lib-nodejs');
}

// _enableNami :: Effect (Promise NamiConnection)
exports._enableNami = () => window.cardano.nami.enable();

// _getNamiAddress :: NamiConnection -> Effect (Promise String)
exports._getNamiAddress = nami => () =>
  nami.getUsedAddresses().then((addrs) => addrs[0]);

// _getNamiCollateral
//   :: MaybeFfiHelper
//   -> NamiConnection
//   -> Effect (Promise String)
exports._getNamiCollateral = maybe => nami => () =>
  nami.experimental.getCollateral().then((utxos) => {
  return utxos.length ? maybe.just(utxos[0]) : maybe.nothing;
});

// _signTxNami :: String -> NamiConnection -> Effect (Promise String)
exports._signTxNami = txHex => nami => () => {
  return nami.signTx(txHex, true)
      .catch(e => {
          console.log("Error in signTxNami: ", e);
          throw (JSON.stringify(e));
      });
}

// _enableGero :: Effect (Promise GeroConnection)
exports._enableGero = () => window.cardano.gerowallet.enable();

// _getGeroAddress :: GeroConnection -> Effect (Promise String)
exports._getGeroAddress = gero => () =>
  gero.getUsedAddresses().then((addrs) => addrs[0]);

// _getGeroCollateral
//   :: MaybeFfiHelper
//   -> GeroConnection
//   -> Effect (Promise String)
exports._getGeroCollateral = maybe => gero => () =>
  gero.experimental.getCollateral().then((utxos) => {
  return utxos.length ? maybe.just(utxos[0]) : maybe.nothing;
});

// _signTxGero :: String -> GeroConnection -> Effect (Promise String)
exports._signTxGero = txHex => gero => () => {
  return gero.signTx(txHex, true)
      .catch(e => {
          console.log("Error in signTxGero: ", e);
          throw (JSON.stringify(e));
      });
}

// foreign import _attachSignature
//   :: ByteArray
//   -> ByteArray
//   -> Effect (ByteArray)
exports._attachSignature = txBytes => witBytes => () => {
  const tx = lib.Transaction.from_bytes(txBytes);
  const newWits = lib.TransactionWitnessSet.from_bytes(witBytes);
  // .vkeys() may return undefined
  const oldvkeyWits = tx.witness_set().vkeys() || lib.Vkeywitnesses.new();
  const newvkeyWits = newWits.vkeys() || lib.Vkeywitnesses.new();

  // Add old vkeyWits into the new set as well to make multi-sign possible
  for (let i = 0; i < oldvkeyWits.len(); i++) {
    newvkeyWits.add(oldvkeyWits.get(i));
  }

  const wits = tx.witness_set();
  // If there are no vkeys in either witness set, we don't want to attach
  // empty vkeys to tx witnesses
  // (So in this case oldvkeyWits remain untouched).
  if (newvkeyWits.len() != 0) {
    wits.set_vkeys(newvkeyWits);
  }

  return lib.Transaction.new(tx.body(), wits, tx.auxiliary_data()).to_bytes();
};
