// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1830314472"] = [{"values":[{"sourceSpan":{"start":[71,1],"name":".spago/debug/v6.0.2/src/Debug.purs","end":[71,66]},"score":0,"packageInfo":{"values":["debug"],"tag":"Package"},"name":"traceTime","moduleName":"Debug","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Debug"],"DebugWarning"],"constraintArgs":[]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},{"tag":"TypeVar","contents":"a"}]}}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Measures the time it takes the given function to run and prints it out,\nthen returns the function's result. This is handy for diagnosing\nperformance problems by wrapping suspected parts of the code in\n`traceTime`.\n\nFor example:\n```purescript\nbunchOfThings =\n  [ traceTime \"one\" \\_ -> one x y\n  , traceTime \"two\" \\_ -> two z\n  , traceTime \"three\" \\_ -> three a b c\n  ]\n```\n\nConsole output would look something like this:\n```\none took 3.456ms\ntwo took 562.0023ms\nthree took 42.0111ms\n```\n\nNote that the timing precision may differ depending on whether the\nPerformance API is supported. Where supported (on most modern browsers and\nversions of Node), the Performance API offers timing resolution of 5\nmicroseconds. Where Performance API is not supported, this function will\nfall back on standard JavaScript Date object, which only offers a\n1-millisecond resolution.\n"}],"tag":"SearchResult"}]