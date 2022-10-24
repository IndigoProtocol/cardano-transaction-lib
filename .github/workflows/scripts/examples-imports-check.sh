#!/bin/sh

examples_imports_check() {
  local examples="$HOME/purs/cardano-transaction-lib/examples"
  local examples_with_internal_imports=""

  for example_purs in $(find $examples -type f -name *.purs ! -path "$examples/KeyWallet/Internal/*"); do
    if $(grep -q '^import Ctl.Internal.*' "$example_purs"); then
      examples_with_internal_imports+="\n$(basename $example_purs)"
    fi
  done

  if [ "$examples_with_internal_imports" ]; then
    echo 'The following examples import internal CTL modules:'
    printf "$examples_with_internal_imports"
    exit 1
  fi
} 
examples_imports_check

