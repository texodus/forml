#!/bin/bash

cabal build
./dist/build/sonnet/sonnet src/sonnet/prelude.formal
java -jar ~/Downloads/compiler-latest/compiler.jar --js prelude.js --js_output_file prelude2.js
~/opt/js-beautify/python/js-beautify prelude2.js > prelude.js
rm prelude2.js
