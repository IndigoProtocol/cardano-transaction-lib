/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

exports._mkPlutusData_bytes = bytes => lib.PlutusData.new_bytes(bytes);
exports._mkPlutusData_list = list => lib.PlutusData.new_list(list);
exports._mkPlutusData_map = map => lib.PlutusData.new_map(map);
exports._mkPlutusData_datumMap = cbor => lib.PlutusData.new_datum_map(cbor);
exports._mkPlutusData_integer = int => lib.PlutusData.new_integer(int);
exports._mkPlutusData_constr = constr =>
  lib.PlutusData.new_constr_plutus_data(constr);

exports._packPlutusList = containerHelper => elems =>
  containerHelper.pack(lib.PlutusList, elems);
exports._mkConstrPlutusData = n => list => lib.ConstrPlutusData.new(n, list);

exports._bigIntFromString = maybe => str => {
  // this is needed because try/catch overuse breaks runtime badly
  // https://github.com/Plutonomicon/cardano-transaction-lib/issues/875
  try {
    BigInt(str);
    return maybe.just(lib.BigInt.from_str(str));
  } catch (_) {
    return maybe.nothing;
  }
};

exports._packMap = first => second => kvs => {
  const res = lib.PlutusMap.new();
  for (let kv of kvs) {
    res.insert(first(kv), second(kv));
  }
  return res;
};

exports._packDatumMap = first => second => kvs => {
  const res = lib.PlutusDatumMap.new();
  for (let kv of kvs) {
    res.insert(first(kv), second(kv));
  }
  return res;
};
