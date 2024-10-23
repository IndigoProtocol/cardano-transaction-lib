// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1272466186"] = [{"values":[{"sourceSpan":{"start":[444,1],"name":".spago/parsing/v10.2.0/src/Parsing.purs","end":[444,85]},"score":1,"packageInfo":{"values":["parsing"],"tag":"Package"},"name":"region","moduleName":"Parsing","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["s",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Parsing"],"ParseError"]}]},{"tag":"TypeConstructor","contents":[["Parsing"],"ParseError"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Parsing"],"ParserT"]},{"tag":"TypeVar","contents":"s"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Parsing"],"ParserT"]},{"tag":"TypeVar","contents":"s"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Contextualize parsing failures inside a region. If a parsing failure\noccurs, then the `ParseError` will be transformed by each containing\n`region` as the parser backs out the call stack.\n\nFor example, here’s a helper function `inContext` which uses `region` to\nadd some string context to the error messages.\n\n```\nlet\n  inContext :: forall s m a. (String -> String) -> ParserT s m a -> ParserT s m a\n  inContext context = region \\(ParseError message pos) ->\n    ParseError (context message) pos\n\n  input = \"Tokyo thirty-nine million\"\n\nlmap (parseErrorHuman input 30) $ runParser input do\n  inContext (\"Megacity list: \" <> _) do\n    cityname <- inContext (\"city name: \" <> _) (takeWhile isLetter)\n    skipSpaces\n    population <- inContext (\"population: \" <> _) intDecimal\n    pure $ Tuple cityname population\n```\n---\n```\nMegacity list: population: Expected Int at position index:6 (line:1, column:7)\n      ▼\nTokyo thirty-nine million\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[53,1],"name":".spago/transformers/v6.0.0/src/Control/Monad/State/Trans.purs","end":[53,69]},"score":24,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"withStateT","moduleName":"Control.Monad.State.Trans","info":{"values":[{"type":{"tag":"ForAll","contents":["s",{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"s"}]},{"tag":"TypeVar","contents":"s"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","State","Trans"],"StateT"]},{"tag":"TypeVar","contents":"s"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","State","Trans"],"StateT"]},{"tag":"TypeVar","contents":"s"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Modify the final state in a `StateT` monad action.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[45,1],"name":".spago/transformers/v6.0.0/src/Control/Comonad/Traced/Class.purs","end":[45,80]},"score":24,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"censor","moduleName":"Control.Comonad.Traced.Class","info":{"values":[{"type":{"tag":"ForAll","contents":["w",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["t",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Functor"],"Functor"],"constraintArgs":[{"tag":"TypeVar","contents":"w"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"t"}]},{"tag":"TypeVar","contents":"t"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Comonad","Traced","Trans"],"TracedT"]},{"tag":"TypeVar","contents":"t"}]},{"tag":"TypeVar","contents":"w"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Comonad","Traced","Trans"],"TracedT"]},{"tag":"TypeVar","contents":"t"}]},{"tag":"TypeVar","contents":"w"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Apply a function to the current position.\n"}],"tag":"SearchResult"}]