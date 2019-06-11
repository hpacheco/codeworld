#!/bin/bash

cwd=$(pwd)

source base.sh

run . cabal install --ghcjs roshask/ roshask/msgs/* gloss-window/ rosy/ codeworld-api-gloss/
