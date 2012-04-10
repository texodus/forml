#!/bin/bash

cabal build
pandoc --smart --to=html src/hs/Sonnet.lhs > sonnet.html
./dist/build/sonnet/sonnet src/sonnet/test.sonnet