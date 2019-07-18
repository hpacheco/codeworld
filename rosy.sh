#!/bin/bash

cwd=$(pwd)

source base.sh

run . cabal install rosy/prepro/
run . cabal_install rosy/prepro/
run . cabal install --ghcjs roshask/ roshask/msgs/* gloss-window/ rosy/ rosy-base/ codeworld-api-gloss/
run rosy/ cabal haddock
run . cabal_install --ghcjs roshask/ roshask/msgs/* gloss-window/ rosy/ rosy-base/ codeworld-api-gloss/
