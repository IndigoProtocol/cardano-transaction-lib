// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["402306724"] = [{"values":[{"sourceSpan":{"start":[55,1],"name":"src/Internal/Plutip/Types.purs","end":[71,4]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"PlutipConfig","moduleName":"Ctl.Internal.Plutip.Types","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["host",{"tag":"TypeConstructor","contents":[["Prim"],"String"]},{"tag":"RCons","contents":["port",{"tag":"TypeConstructor","contents":[["Data","UInt"],"UInt"]},{"tag":"RCons","contents":["logLevel",{"tag":"TypeConstructor","contents":[["Data","Log","Level"],"LogLevel"]},{"tag":"RCons","contents":["ogmiosConfig",{"tag":"TypeConstructor","contents":[["Ctl","Internal","ServerConfig"],"ServerConfig"]},{"tag":"RCons","contents":["kupoConfig",{"tag":"TypeConstructor","contents":[["Ctl","Internal","ServerConfig"],"ServerConfig"]},{"tag":"RCons","contents":["customLogger",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Log","Level"],"LogLevel"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Log","Message"],"Message"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}}]},{"tag":"RCons","contents":["suppressLogs",{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]},{"tag":"RCons","contents":["hooks",{"tag":"TypeConstructor","contents":[["Ctl","Internal","Contract","Hooks"],"Hooks"]},{"tag":"RCons","contents":["clusterConfig",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["slotLength",{"tag":"TypeConstructor","contents":[["Data","Time","Duration"],"Seconds"]},{"tag":"RCons","contents":["epochSize",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","UInt"],"UInt"]}]},{"tag":"RCons","contents":["maxTxSize",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","UInt"],"UInt"]}]},{"tag":"RCons","contents":["raiseExUnitsToMax",{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]},{"tag":"REmpty","contents":{}}]}]}]}]}]},{"tag":"REmpty","contents":{}}]}]}]}]}]}]}]}]}]}]},"arguments":[]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":"A config that is used to run tests on Plutip clusters.\nNote that the test suite starts the services on the specified ports.\nIt does not expect them to be running.\n"}],"tag":"SearchResult"}]