// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["613013591"] = [{"values":[{"sourceSpan":{"start":[290,1],"name":".spago/node-child-process/v9.0.0/src/Node/ChildProcess.purs","end":[294,25]},"score":0,"packageInfo":{"values":["node-child-process"],"tag":"Package"},"name":"exec","moduleName":"Node.ChildProcess","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Node","ChildProcess"],"ExecOptions"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Node","ChildProcess"],"ExecResult"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Node","ChildProcess"],"ChildProcess"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Similar to `spawn`, except that this variant will:\n* run the given command with the shell,\n* buffer output, and wait until the process has exited before calling the\n  callback.\n\nNote that the child process will be killed if the amount of output exceeds\na certain threshold (the default is defined by Node.js).\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[66,1],"name":".spago/effect/v4.0.0/src/Effect.purs","end":[66,73]},"score":54,"packageInfo":{"values":["effect"],"tag":"Package"},"name":"forE","moduleName":"Effect","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Loop over a consecutive collection of numbers.\n\n`forE lo hi f` runs the computation returned by the function `f` for each\nof the inputs between `lo` (inclusive) and `hi` (exclusive).\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[43,1],"name":"src/Internal/Test/E2E/Feedback/Node.purs","end":[47,14]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"subscribeToBrowserEvents","moduleName":"Ctl.Internal.Test.E2E.Feedback.Node","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Toppokki"],"Page"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Test","E2E","Feedback"],"BrowserEvent"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"React to events raised by the browser\n"}],"tag":"SearchResult"}]