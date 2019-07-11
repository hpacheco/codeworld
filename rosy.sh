#!/bin/bash

cwd=$(pwd)

source base.sh

run . git submodule update --recursive --remote
#run roshask/ git pull
#run roshask/  git submodule update --recursive --remote
#run gloss-window/ git pull
#run rosy/ git pull
#run codeworld-api-gloss/ git pull
run . cabal install --ghcjs roshask/ roshask/msgs/* gloss-window/ rosy/ rosy-base/ codeworld-api-gloss/
run rosy/ cabal haddock
