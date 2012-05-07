#!/bin/bash

cabal build
./dist/build/sonnet/sonnet src/sonnet/prelude.formal
java -jar ./lib/compiler.jar --js prelude.js --js_output_file prelude2.js --compilation_level ADVANCED_OPTIMIZATIONS --formatting=pretty_print
mv prelude2.js prelude.js
