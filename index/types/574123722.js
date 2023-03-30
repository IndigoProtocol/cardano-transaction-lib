// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["574123722"] = [{"values":[{"sourceSpan":{"start":[125,1],"name":"src/Internal/Test/E2E/Options.purs","end":[130,4]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"ExtensionOptions","moduleName":"Ctl.Internal.Test.E2E.Options","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["crxFile",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Test","E2E","Types"],"CrxFilePath"]}]},{"tag":"RCons","contents":["password",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Test","E2E","Types"],"WalletPassword"]}]},{"tag":"RCons","contents":["extensionId",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Test","E2E","Types"],"ExtensionId"]}]},{"tag":"RCons","contents":["crxUrl",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Affjax"],"URL"]}]},{"tag":"REmpty","contents":{}}]}]}]}]}]},"arguments":[]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":"Wallet extension options. Everything is wrapped in maybe, because we want\nto be able to override every single environment variable via the CLI.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[139,1],"name":"src/Internal/BalanceTx/RedeemerIndex.purs","end":[144,4]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"RedeemersContext","moduleName":"Ctl.Internal.BalanceTx.RedeemerIndex","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["inputs",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Transaction"],"TransactionInput"]}]},{"tag":"RCons","contents":["mintingPolicyHashes",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Scripts"],"MintingPolicyHash"]}]},{"tag":"RCons","contents":["rewardAddresses",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","RewardAddress"],"RewardAddress"]}]},{"tag":"RCons","contents":["certs",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"Certificate"]}]},{"tag":"REmpty","contents":{}}]}]}]}]}]},"arguments":[]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":"Contains parts of a transaction that are important when indexing redeemers\n"}],"tag":"SearchResult"}]