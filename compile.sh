#!/bin/sh
set -e
cabal-dev --sandbox ../cabal-dev clean
cabal-dev --sandbox ../cabal-dev configure
cabal-dev --sandbox ../cabal-dev build


