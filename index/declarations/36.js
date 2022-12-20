// This file was generated by purescript-docs-search.
window.DocsSearchIndex["36"] = [["~<",[{"values":[{"sourceSpan":{"start":[14,1],"name":".spago/pipes/v7.0.1/src/Pipes.purs","end":[14,34]},"score":1,"packageInfo":{"values":["pipes"],"tag":"Package"},"name":"(~<)","moduleName":"Pipes","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["~<<",[{"values":[{"sourceSpan":{"start":[126,1],"name":".spago/pipes/v7.0.1/src/Pipes/Core.purs","end":[126,40]},"score":1,"packageInfo":{"values":["pipes"],"tag":"Package"},"name":"(~<<)","moduleName":"Pipes.Core","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["|>=",[{"values":[{"sourceSpan":{"start":[90,1],"name":".spago/mmorph/v6.0.0/src/Control/Monad/Morph.purs","end":[90,26]},"score":1,"packageInfo":{"values":["mmorph"],"tag":"Package"},"name":"(|>=)","moduleName":"Control.Monad.Morph","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txbinaryfixture1",[{"values":[{"sourceSpan":{"start":[774,1],"name":"test/Fixtures.purs","end":[774,27]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txBinaryFixture1","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Prim"],"String"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"To quickly check a serialized tx, create a file with the following contents:\n\n\n```\n{\n  \"type\": \"Tx AlonzoEra\",\n  \"description\": \"\",\n  \"cborHex\": ...\n}\n```\n\nAnd call `cardano-cli transaction view --tx-file ./that-file`\n"}],"tag":"SearchResult"}]],["txbinaryfixture2",[{"values":[{"sourceSpan":{"start":[781,1],"name":"test/Fixtures.purs","end":[781,27]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txBinaryFixture2","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Prim"],"String"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txbinaryfixture3",[{"values":[{"sourceSpan":{"start":[789,1],"name":"test/Fixtures.purs","end":[789,27]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txBinaryFixture3","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Prim"],"String"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txbinaryfixture4",[{"values":[{"sourceSpan":{"start":[798,1],"name":"test/Fixtures.purs","end":[798,27]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txBinaryFixture4","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Prim"],"String"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txbinaryfixture5",[{"values":[{"sourceSpan":{"start":[857,1],"name":"test/Fixtures.purs","end":[857,27]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txBinaryFixture5","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Prim"],"String"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txbinaryfixture6",[{"values":[{"sourceSpan":{"start":[864,1],"name":"test/Fixtures.purs","end":[864,27]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txBinaryFixture6","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Prim"],"String"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txbody",[{"values":[{"sourceSpan":{"start":[239,1],"name":"src/Internal/Cardano/Types/Transaction.purs","end":[257,4]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxBody","moduleName":"Ctl.Internal.Cardano.Types.Transaction","info":{"values":[{"typeArguments":[],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":200000,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxBody","moduleName":"Ctl.Internal.Cardano.Types.Transaction","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["inputs",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Set"],"Set"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Transaction"],"TransactionInput"]}]},{"tag":"RCons","contents":["outputs",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"TransactionOutput"]}]},{"tag":"RCons","contents":["fee",{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Value"],"Coin"]},{"tag":"RCons","contents":["ttl",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Serialization","Address"],"Slot"]}]},{"tag":"RCons","contents":["certs",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"Certificate"]}]}}]},{"tag":"RCons","contents":["withdrawals",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Map","Internal"],"Map"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","RewardAddress"],"RewardAddress"]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Value"],"Coin"]}]}}]},{"tag":"RCons","contents":["update",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"Update"]}]},{"tag":"RCons","contents":["auxiliaryDataHash",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"AuxiliaryDataHash"]}]},{"tag":"RCons","contents":["validityStartInterval",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Serialization","Address"],"Slot"]}]},{"tag":"RCons","contents":["mint",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"Mint"]}]},{"tag":"RCons","contents":["scriptDataHash",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"ScriptDataHash"]}]},{"tag":"RCons","contents":["collateral",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Transaction"],"TransactionInput"]}]}}]},{"tag":"RCons","contents":["requiredSigners",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"RequiredSigner"]}]}}]},{"tag":"RCons","contents":["networkId",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Serialization","Address"],"NetworkId"]}]},{"tag":"RCons","contents":["collateralReturn",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"TransactionOutput"]}]},{"tag":"RCons","contents":["totalCollateral",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Value"],"Coin"]}]},{"tag":"RCons","contents":["referenceInputs",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Set"],"Set"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Transaction"],"TransactionInput"]}]},{"tag":"REmpty","contents":{}}]}]}]}]}]}]}]}]}]}]}]}]}]}]}]}]}]}]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"TxBody"]}]},"dataDeclType":"newtype"}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txconstraint",[{"values":[{"sourceSpan":{"start":[160,1],"name":"src/Internal/Types/TxConstraints.purs","end":[197,19]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxConstraint","moduleName":"Ctl.Internal.Types.TxConstraints","info":{"values":[{"typeArguments":[],"dataDeclType":"data"}],"tag":"DataResult"},"hashAnchor":"t","comments":"Constraints on transactions that want to spend script outputs\n"}],"tag":"SearchResult"}]],["txconstraints",[{"values":[{"sourceSpan":{"start":[274,1],"name":"src/Internal/Types/TxConstraints.purs","end":[278,4]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxConstraints","moduleName":"Ctl.Internal.Types.TxConstraints","info":{"values":[{"typeArguments":[["i",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}],["o",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"Restrictions placed on the allocation of funds to outputs of transactions.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":200000,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxConstraints","moduleName":"Ctl.Internal.Types.TxConstraints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["constraints",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"TxConstraint"]}]},{"tag":"RCons","contents":["ownInputs",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"InputConstraint"]},{"tag":"TypeVar","contents":"i"}]}}]},{"tag":"RCons","contents":["ownOutputs",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"OutputConstraint"]},{"tag":"TypeVar","contents":"o"}]}}]},{"tag":"REmpty","contents":{}}]}]}]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"TxConstraints"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"o"}]}]},"dataDeclType":"newtype"}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txevaluationfailure",[{"values":[{"sourceSpan":{"start":[871,1],"name":"src/Internal/QueryM/Ogmios.purs","end":[873,63]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxEvaluationFailure","moduleName":"Ctl.Internal.QueryM.Ogmios","info":{"values":[{"typeArguments":[],"dataDeclType":"data"}],"tag":"DataResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"}]],["txevaluationr",[{"values":[{"sourceSpan":{"start":[789,1],"name":"src/Internal/QueryM/Ogmios.purs","end":[790,50]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxEvaluationR","moduleName":"Ctl.Internal.QueryM.Ogmios","info":{"values":[{"typeArguments":[],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":200000,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxEvaluationR","moduleName":"Ctl.Internal.QueryM.Ogmios","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"TxEvaluationFailure"]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"TxEvaluationResult"]}]}}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"TxEvaluationR"]}]},"dataDeclType":"newtype"}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txevaluationresult",[{"values":[{"sourceSpan":{"start":[802,1],"name":"src/Internal/QueryM/Ogmios.purs","end":[803,39]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxEvaluationResult","moduleName":"Ctl.Internal.QueryM.Ogmios","info":{"values":[{"typeArguments":[],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":200000,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxEvaluationResult","moduleName":"Ctl.Internal.QueryM.Ogmios","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Map","Internal"],"Map"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"RedeemerPointer"]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"ExecutionUnits"]}]}}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"TxEvaluationResult"]}]},"dataDeclType":"newtype"}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txfixture1",[{"values":[{"sourceSpan":{"start":[418,1],"name":"test/Fixtures.purs","end":[418,26]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txFixture1","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"Transaction"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txfixture2",[{"values":[{"sourceSpan":{"start":[452,1],"name":"test/Fixtures.purs","end":[452,26]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txFixture2","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"Transaction"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txfixture3",[{"values":[{"sourceSpan":{"start":[486,1],"name":"test/Fixtures.purs","end":[486,26]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txFixture3","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"Transaction"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txfixture4",[{"values":[{"sourceSpan":{"start":[545,1],"name":"test/Fixtures.purs","end":[545,26]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txFixture4","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"Transaction"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txfixture5",[{"values":[{"sourceSpan":{"start":[674,1],"name":"test/Fixtures.purs","end":[674,26]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txFixture5","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"Transaction"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txfixture6",[{"values":[{"sourceSpan":{"start":[721,1],"name":"test/Fixtures.purs","end":[721,26]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txFixture6","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"Transaction"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txhash",[{"values":[{"sourceSpan":{"start":[396,1],"name":"src/Internal/QueryM/Ogmios.purs","end":[396,24]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxHash","moduleName":"Ctl.Internal.QueryM.Ogmios","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","ByteArray"],"ByteArray"]},"arguments":[]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"}]],["txinputfixture1",[{"values":[{"sourceSpan":{"start":[1191,1],"name":"test/Fixtures.purs","end":[1191,36]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txInputFixture1","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Transaction"],"TransactionInput"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txoutputbinaryfixture1",[{"values":[{"sourceSpan":{"start":[274,1],"name":"test/Fixtures.purs","end":[274,33]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txOutputBinaryFixture1","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Prim"],"String"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txoutputfixture1",[{"values":[{"sourceSpan":{"start":[219,1],"name":"test/Fixtures.purs","end":[219,38]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txOutputFixture1","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"TransactionOutput"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txoutputfixture2",[{"values":[{"sourceSpan":{"start":[242,1],"name":"test/Fixtures.purs","end":[242,38]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txOutputFixture2","moduleName":"Test.Ctl.Fixtures","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"TransactionOutput"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txoutrefcache",[{"values":[{"sourceSpan":{"start":[51,1],"name":"src/Internal/Types/UsedTxOuts.purs","end":[51,52]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxOutRefCache","moduleName":"Ctl.Internal.Types.UsedTxOuts","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Map","Internal"],"Map"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Transaction"],"TransactionHash"]}]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Set"],"Set"]},{"tag":"TypeConstructor","contents":[["Data","UInt"],"UInt"]}]}}]},"arguments":[]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"}]],["txoutrefnotfound",[{"values":[{"sourceSpan":null,"score":200000,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxOutRefNotFound","moduleName":"Ctl.Internal.Types.ScriptLookups","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Transaction"],"TransactionInput"]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","ScriptLookups"],"MkUnbalancedTxError"]}]},"dataDeclType":"data"}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txoutreftotransactioninput",[{"values":[{"sourceSpan":{"start":[46,1],"name":"src/Internal/TxOutput.purs","end":[47,65]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txOutRefToTransactionInput","moduleName":"Ctl.Internal.TxOutput","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"OgmiosTxOutRef"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Transaction"],"TransactionInput"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"A module for helpers of the various transaction output types.\nConverts an Ogmios transaction input to (internal) `TransactionInput`\n"}],"tag":"SearchResult"}]],["txoutrefunlockkeys",[{"values":[{"sourceSpan":{"start":[52,1],"name":"src/Internal/Types/UsedTxOuts.purs","end":[52,62]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxOutRefUnlockKeys","moduleName":"Ctl.Internal.Types.UsedTxOuts","info":{"values":[{"typeArguments":[],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":200000,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxOutRefUnlockKeys","moduleName":"Ctl.Internal.Types.UsedTxOuts","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","UsedTxOuts"],"TxOutRefCache"]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","UsedTxOuts"],"TxOutRefUnlockKeys"]}]},"dataDeclType":"newtype"}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txoutrefwrongtype",[{"values":[{"sourceSpan":null,"score":200000,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"TxOutRefWrongType","moduleName":"Ctl.Internal.Types.ScriptLookups","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Transaction"],"TransactionInput"]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","ScriptLookups"],"MkUnbalancedTxError"]}]},"dataDeclType":"data"}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["txwitnesssetsetplutusscripts",[{"values":[{"sourceSpan":{"start":[175,1],"name":"src/Internal/Serialization/WitnessSet.purs","end":[176,59]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"txWitnessSetSetPlutusScripts","moduleName":"Ctl.Internal.Serialization.WitnessSet","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Serialization","Types"],"TransactionWitnessSet"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Serialization","Types"],"PlutusScripts"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["symbol",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"Symbol","moduleName":"Prim","info":{"values":[{"kind":{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"`Symbol` is the kind of type-level strings.\n\nConstruct types of this kind using the same literal syntax as documented\nfor strings.\n"}],"tag":"SearchResult"}]],["symbols",[{"values":[{"sourceSpan":{"start":[206,1],"name":"src/Internal/Plutus/Types/Value.purs","end":[206,41]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"symbols","moduleName":"Ctl.Internal.Plutus.Types.Value","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutus","Types","Value"],"Value"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutus","Types","CurrencySymbol"],"CurrencySymbol"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The list of `CurrencySymbol`s of a `Value`.\n"}],"tag":"SearchResult"}]],["symlink",[{"values":[{"sourceSpan":{"start":[122,1],"name":".spago/node-fs-aff/v7.0.0/src/Node/FS/Aff.purs","end":[125,20]},"score":1,"packageInfo":{"values":["node-fs-aff"],"tag":"Package"},"name":"symlink","moduleName":"Node.FS.Aff","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Node","Path"],"FilePath"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Node","Path"],"FilePath"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Node","FS"],"SymlinkType"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"\nCreates a symlink.\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[150,1],"name":".spago/node-fs/v6.2.0/src/Node/FS/Async.purs","end":[154,23]},"score":3,"packageInfo":{"values":["node-fs"],"tag":"Package"},"name":"symlink","moduleName":"Node.FS.Async","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Node","Path"],"FilePath"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Node","Path"],"FilePath"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Node","FS"],"SymlinkType"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Node","FS","Async"],"Callback"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Creates a symlink.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[133,1],"name":".spago/node-fs/v6.2.0/src/Node/FS/Sync.purs","end":[136,23]},"score":3,"packageInfo":{"values":["node-fs"],"tag":"Package"},"name":"symlink","moduleName":"Node.FS.Sync","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Node","Path"],"FilePath"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Node","Path"],"FilePath"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Node","FS"],"SymlinkType"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Creates a symlink.\n"}],"tag":"SearchResult"}]],["symlinktype",[{"values":[{"sourceSpan":{"start":[63,1],"name":".spago/node-fs/v6.2.0/src/Node/FS.purs","end":[63,53]},"score":3,"packageInfo":{"values":["node-fs"],"tag":"Package"},"name":"SymlinkType","moduleName":"Node.FS","info":{"values":[{"typeArguments":[],"dataDeclType":"data"}],"tag":"DataResult"},"hashAnchor":"t","comments":"Symlink varieties.\n"}],"tag":"SearchResult"}]],["symlinktypetonode",[{"values":[{"sourceSpan":{"start":[67,1],"name":".spago/node-fs/v6.2.0/src/Node/FS.purs","end":[67,43]},"score":3,"packageInfo":{"values":["node-fs"],"tag":"Package"},"name":"symlinkTypeToNode","moduleName":"Node.FS","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Node","FS"],"SymlinkType"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Convert a `SymlinkType` to a `String` in the format expected by the\nNode.js filesystem API.\n"}],"tag":"SearchResult"}]],["symmetricclosure",[{"values":[{"sourceSpan":{"start":[284,1],"name":".spago/medea/8b215851959aa8bbf33e6708df6bd683c89d1a5a/src/Data/AdjacencyMap.purs","end":[284,72]},"score":0,"packageInfo":{"values":["medea"],"tag":"Package"},"name":"symmetricClosure","moduleName":"Data.AdjacencyMap","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","AdjacencyMap"],"AdjacencyMap"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","AdjacencyMap"],"AdjacencyMap"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["sync",[{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["web-html"],"tag":"Package"},"name":"Sync","moduleName":"Web.HTML.HTMLImageElement.DecodingHint","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Web","HTML","HTMLImageElement","DecodingHint"],"DecodingHint"]},"dataDeclType":"data"}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["systemerror",[{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["exitcodes"],"tag":"Package"},"name":"SystemError","moduleName":"ExitCodes","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["ExitCodes"],"ExitCode"]},"dataDeclType":"data"}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["systemid",[{"values":[{"sourceSpan":{"start":[32,1],"name":".spago/web-dom/v5.0.0/src/Web/DOM/DocumentType.purs","end":[32,50]},"score":3,"packageInfo":{"values":["web-dom"],"tag":"Package"},"name":"systemId","moduleName":"Web.DOM.DocumentType","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Web","DOM","DocumentType"],"DocumentType"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["systemstart",[{"values":[{"sourceSpan":{"start":[407,1],"name":"src/Internal/QueryM/Ogmios.purs","end":[407,41]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"SystemStart","moduleName":"Ctl.Internal.QueryM.Ogmios","info":{"values":[{"typeArguments":[],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":200000,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"SystemStart","moduleName":"Ctl.Internal.QueryM.Ogmios","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"SystemStart"]}]},"dataDeclType":"newtype"}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["systemstartfixture",[{"values":[{"sourceSpan":{"start":[87,1],"name":"test/Types/Interval.purs","end":[87,41]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"systemStartFixture","moduleName":"Test.Ctl.Types.Interval","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"SystemStart"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],[">|>",[{"values":[{"sourceSpan":{"start":[86,1],"name":".spago/mmorph/v6.0.0/src/Control/Monad/Morph.purs","end":[86,36]},"score":1,"packageInfo":{"values":["mmorph"],"tag":"Package"},"name":"(>|>)","moduleName":"Control.Monad.Morph","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["<~",[{"values":[{"sourceSpan":{"start":[11,1],"name":".spago/pipes/v7.0.1/src/Pipes.purs","end":[11,34]},"score":1,"packageInfo":{"values":["pipes"],"tag":"Package"},"name":"(<~)","moduleName":"Pipes","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["<~<",[{"values":[{"sourceSpan":{"start":[122,1],"name":".spago/pipes/v7.0.1/src/Pipes/Core.purs","end":[122,40]},"score":1,"packageInfo":{"values":["pipes"],"tag":"Package"},"name":"(<~<)","moduleName":"Pipes.Core","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["+++",[{"values":[{"sourceSpan":{"start":[56,1],"name":".spago/profunctor/v5.0.0/src/Data/Profunctor/Choice.purs","end":[56,28]},"score":4,"packageInfo":{"values":["profunctor"],"tag":"Package"},"name":"(+++)","moduleName":"Data.Profunctor.Choice","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["$",[{"values":[{"sourceSpan":{"start":[62,1],"name":".spago/prelude/v5.0.1/src/Data/Function.purs","end":[62,20]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"($)","moduleName":"Data.Function","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":"Applies a function to an argument: the reverse of `(#)`.\n\n```purescript\nlength $ groupBy productCategory $ filter isInStock $ products\n```\n\nis equivalent to:\n\n```purescript\nlength (groupBy productCategory (filter isInStock products))\n```\n\nOr another alternative equivalent, applying chain of composed functions to\na value:\n\n```purescript\nlength <<< groupBy productCategory <<< filter isInStock $ products\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[12,1],"name":".spago/typelevel-prelude/v6.0.0/src/Type/Function.purs","end":[12,25]},"score":4,"packageInfo":{"values":["typelevel-prelude"],"tag":"Package"},"name":"type ($)","moduleName":"Type.Function","info":{"values":[],"tag":"TypeAliasResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"}]]]