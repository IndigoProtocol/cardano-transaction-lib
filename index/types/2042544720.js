// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["2042544720"] = [{"values":[{"sourceSpan":{"start":[112,1],"name":".spago/profunctor-lenses/v7.0.1/src/Data/Lens/Traversal.purs","end":[117,21]},"score":1,"packageInfo":{"values":["profunctor-lenses"],"tag":"Package"},"name":"element","moduleName":"Data.Lens.Traversal","info":{"values":[{"type":{"tag":"ForAll","contents":["p",{"tag":"ForAll","contents":["s",{"tag":"ForAll","contents":["t",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Lens","Internal","Wander"],"Wander"],"constraintArgs":[{"tag":"TypeVar","contents":"p"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Lens","Types"],"Traversal"]},{"tag":"TypeVar","contents":"s"}]},{"tag":"TypeVar","contents":"t"}]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Lens","Types"],"Optic"]},{"tag":"TypeVar","contents":"p"}]},{"tag":"TypeVar","contents":"s"}]},{"tag":"TypeVar","contents":"t"}]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Combine an index and a traversal to narrow the focus to a single\nelement. Compare to `Data.Lens.Index`. \n\n```purescript\nset     (element 2 traversed) 8888 [0, 0, 3] == [0, 0, 8888]\npreview (element 2 traversed)      [0, 0, 3] == Just 3\n```\nThe resulting traversal is called an *affine traversal*, which\nmeans that the traversal focuses on one or zero (if the index is out of range)\nresults.\n"}],"tag":"SearchResult"}]