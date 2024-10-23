// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1130185241"] = [{"values":[{"sourceSpan":{"start":[141,1],"name":"src/Internal/CoinSelection/UtxoIndex.purs","end":[142,76]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"utxoIndexPartition","moduleName":"Ctl.Internal.CoinSelection.UtxoIndex","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Cardano","Types","TransactionInput"],"TransactionInput"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","CoinSelection","UtxoIndex"],"UtxoIndex"]}]},{"tag":"ParensInType","contents":{"tag":"BinaryNoParensType","contents":[{"tag":"TypeOp","contents":[["Data","Tuple","Nested"],"/\\"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","CoinSelection","UtxoIndex"],"UtxoIndex"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","CoinSelection","UtxoIndex"],"UtxoIndex"]}]}}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Partition `UtxoIndex`\n\nTaken from cardano-wallet:\nhttps://github.com/input-output-hk/cardano-wallet/blob/9d73b57e23392e25148cfc8db560cb8f656cb56a/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L344\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[275,1],"name":".spago/cardano-transaction-builder/v2.0.0/src/Cardano/Transaction/Edit.purs","end":[277,50]},"score":0,"packageInfo":{"values":["cardano-transaction-builder"],"tag":"Package"},"name":"editTransactionSafe","moduleName":"Cardano.Transaction.Edit","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Cardano","Types","Transaction"],"Transaction"]}]},{"tag":"TypeConstructor","contents":[["Cardano","Types","Transaction"],"Transaction"]}]}}]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Cardano","Types","Transaction"],"Transaction"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeConstructor","contents":[["Cardano","Types","Redeemer"],"Redeemer"]}]},{"tag":"TypeConstructor","contents":[["Cardano","Types","Transaction"],"Transaction"]}]}]}}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Like `editTransaction`, but fails if:\n\n- the input transaction's redeemers have invalid `index` pointers\n- the resulting transaction's redeemers have invalid `index` pointers\n\nThe first problematic redeemer will be returned as error value.\n"}],"tag":"SearchResult"}]