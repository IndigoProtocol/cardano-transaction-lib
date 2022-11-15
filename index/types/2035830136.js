// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["2035830136"] = [{"values":[{"sourceSpan":{"start":[113,1],"name":".spago/variant/v7.0.3/src/Data/Variant.purs","end":[121,6]},"score":2,"packageInfo":{"values":["variant"],"tag":"Package"},"name":"onMatch","moduleName":"Data.Variant","info":{"values":[{"type":{"tag":"ForAll","contents":["rl",{"tag":"ForAll","contents":["r",{"tag":"ForAll","contents":["r1",{"tag":"ForAll","contents":["r2",{"tag":"ForAll","contents":["r3",{"tag":"ForAll","contents":["b",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Prim","RowList"],"RowToList"],"constraintArgs":[{"tag":"TypeVar","contents":"r"},{"tag":"TypeVar","contents":"rl"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Variant","Internal"],"VariantMatchCases"],"constraintArgs":[{"tag":"TypeVar","contents":"rl"},{"tag":"TypeVar","contents":"r1"},{"tag":"TypeVar","contents":"b"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Prim","Row"],"Union"],"constraintArgs":[{"tag":"TypeVar","contents":"r1"},{"tag":"TypeVar","contents":"r2"},{"tag":"TypeVar","contents":"r3"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"TypeVar","contents":"r"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Variant"],"Variant"]},{"tag":"TypeVar","contents":"r2"}]}]},{"tag":"TypeVar","contents":"b"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Variant"],"Variant"]},{"tag":"TypeVar","contents":"r3"}]}]},{"tag":"TypeVar","contents":"b"}]}]}]}]}]}]},null]},null]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Match a `Variant` with a `Record` containing functions for handling cases.\nThis is similar to `on`, except instead of providing a single label and\nhandler, you can provide a record where each field maps to a particular\n`Variant` case.\n\n```purescript\nonMatch\n  { foo: \\foo -> \"Foo: \" <> foo\n  , bar: \\bar -> \"Bar: \" <> bar\n  }\n````\n\nPolymorphic functions in records (such as `show` or `id`) can lead\nto inference issues if not all polymorphic variables are specified\nin usage. When in doubt, label methods with specific types, such as\n`show :: Int -> String`, or give the whole record an appropriate type.\n"}],"tag":"SearchResult"}]