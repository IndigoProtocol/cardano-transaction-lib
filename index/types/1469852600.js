// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1469852600"] = [{"values":[{"sourceSpan":{"start":[364,1],"name":"src/Internal/Contract/Monad.purs","end":[365,75]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"withContractEnv","moduleName":"Ctl.Internal.Contract.Monad","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Contract","Monad"],"ContractParams"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Contract","Monad"],"ContractEnv"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeVar","contents":"a"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Constructs and finalizes a contract environment that is usable inside a\nbracket callback.\nOne environment can be used by multiple `Contract`s in parallel (see\n`runContractInEnv`).\nMake sure that `Aff` action does not end before all contracts that use the\nruntime terminate. Otherwise `WebSocket`s will be closed too early.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[391,1],"name":"src/Contract/Transaction.purs","end":[395,16]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"withBalancedTx","moduleName":"Contract.Transaction","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","ProcessConstraints","UnbalancedTx"],"UnbalancedTx"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","BalanceTx","Types"],"FinalizedTransaction"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Contract","Monad"],"Contract"]},{"tag":"TypeVar","contents":"a"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Contract","Monad"],"Contract"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Same as `withBalancedTxWithConstraints`, but uses the default balancer\nconstraints.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[295,1],"name":"src/Contract/Test/Assert.purs","end":[299,21]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"assertionToCheck","moduleName":"Contract.Test.Assert","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Contract","Test","Assert"],"ContractAssertion"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Contract","Test","Assert"],"ContractCheck"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create a check that simply asserts something about a `Contract` result.\n\nIf a `Contract` throws an exception, the assertion is never checked,\nbecause the result is never computed. In this case, a warning will be\nprinted, containing the given description.\n"}],"tag":"SearchResult"}]