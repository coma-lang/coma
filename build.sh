#!/usr/bin/bash

#
# This script is a handy way to build this project with cabal and extract the
# binary into the project root folder. 
#
# Unfortunately, I did not find a way to tell cabal where exactly to output the
# binary, so I have to use this hack instead...
#

cabal build
cp ./dist-newstyle/build/x86_64-linux/ghc-8.10.3/Coma-0.1.0.0/x/Coma/build/Coma/Coma ./
