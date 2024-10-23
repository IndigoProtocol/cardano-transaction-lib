// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1593683767"] = [{"values":[{"sourceSpan":{"start":[78,1],"name":".spago/node-process/v10.0.0/src/Node/Process.purs","end":[78,90]},"score":4,"packageInfo":{"values":["node-process"],"tag":"Package"},"name":"onUnhandledRejection","moduleName":"Node.Process","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["b",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Install a handler for unhandled promise rejections. The callback will be\ncalled when the process emits the `unhandledRejection` event.\n\nThe first argument to the handler can be whatever type the unhandled\nPromise yielded on rejection (typically, but not necessarily, an `Error`).\n\nThe handler currently does not expose the type of the second argument,\nwhich is a `Promise`, in order to allow users of this library to choose\ntheir own PureScript `Promise` bindings.\n"}],"tag":"SearchResult"}]