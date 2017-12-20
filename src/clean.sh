#!/usr/bin/env sh
# A simple script to clean *.hi *.o *.dyn_o *.dyn_hi produced due to ghc --make
proj_root="project"

cd ~/$proj_root/GS1Combinators/src
rm *.hi *.o *.dyn_o *.dyn_hi
cd Data/GS1
rm *.hi *.o *.dyn_o *.dyn_hi
cd Parser
rm *.hi *.o *.dyn_o *.dyn_hi
