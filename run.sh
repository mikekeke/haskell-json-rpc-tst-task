#!/usr/bin/env bash
#  stack build --file-watch --fast --exec haskell-json-rpc-exe
stack build --fast
stack exec haskell-json-rpc-exe