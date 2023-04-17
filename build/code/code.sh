#!/usr/bin/env bash

if [ -z "$BUILD_TARGET" ]; then
  echo "Must specify BUILD_TARGET"
  exit 1
fi

set -o xtrace

ln -fs "build/$BUILD_TARGET.cabal.project" cabal.project
echo "Symlinked cabal.project for BUILD_TARGET=$BUILD_TARGET"
