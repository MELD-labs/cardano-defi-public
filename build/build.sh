#!/usr/bin/env bash

if [ -z "$BUILD_TARGET" ]; then
  echo "Must specify BUILD_TARGET"
  exit 1
fi

set -o xtrace

cabal --project-file="build/$BUILD_TARGET.cabal.project" "$@"
