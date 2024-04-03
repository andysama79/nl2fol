#!/usr/bin/bash

cd interpreter

cabal run interpreter -- ../$1

cd ..