/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@mitchycola/cardano-serialization-lib-browser");
} else {
  lib = require("@mitchycola/cardano-serialization-lib-nodejs");
}

exports.newPositive = lib.Int.new;
exports.newNegative = lib.Int.new_negative;
exports._intToStr = n => n.to_str();
