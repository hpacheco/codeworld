#!/bin/bash

cwd=$(pwd)

source base.sh
#run .  ghc-pkg describe ghc
#run . ghcjs-pkg describe ghc

#run . cabal_install --ghcjs build/ghcjs/dist-newstyle/sdist/ghc-api-ghcjs-8.6.5/ghc-api-ghcjs.cabal



#run rosy  cabal configure --ghcjs
#run rosy   cabal haddock --verbose --haddock-html


#run . cabal install rosy/prepro/rosy-pp.cabal
run . cabal_install rosy/prepro/rosy-pp.cabal
#run . cabal install --ghcjs rosy/packages/roshask/roshask.cabal rosy/packages/roshask/msgs/* rosy/packages/gloss-window/ rosy/ rosy/rosy-base/ codeworld-api-gloss/
#run rosy/ cabal haddock

#run . cabal_install ghc-tcplugins-extra-0.3.2
#run . cabal_install --ghcjs ghc-tcplugins-extra/
#run . cabal_install magic-tyfams-0.1.1.0
#run . cabal_install --ghcjs magic-tyfams/

#run . cabal_install --verbose --libdir=/home/haslab/codeworld-rosy/build/.ghcup/ghc/8.6.5/lib --libsubdir=ghc-8.6.5/cmptype-0.2.0.0/ cmptype-0.2.0.0

#run . cabal_install cmptype-0.2.0.0
#run . cabal_install --ghcjs cmptype/

#run . cabal_install base-noprelude
#run . cabal_install --ghcjs base-noprelude-4.12.0.0/

#run . ghcjs -v  -hide-package base -package rosy-base -package base-noprelude -package codeworld-api -package QuickCheck -hide-all-packages -plugin-package cmptype -fplugin=Type.Compare.Plugin program.hs

run . cabal_install --ghcjs rosy/packages/roshask/roshask.cabal rosy/packages/roshask/msgs/* rosy/packages/gloss-window/gloss-window.cabal rosy/rosy.cabal rosy/rosy-base/rosy-base.cabal codeworld-api-gloss/
