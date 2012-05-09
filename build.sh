#!/bin/bash


cabal build
./dist/build/formal/formal src/formal/prelude.formal
java -jar ./lib/compiler.jar --js prelude.js --js_output_file prelude2.js --formatting=pretty_print --compilation_level ADVANCED_OPTIMIZATIONS --warning_level QUIET
cat lib/js/jasmine-1.0.1/jasmine.js prelude2.js prelude.spec.js lib/js/console.js > prelude.node.js
mv prelude2.js prelude.js
node prelude.node.js
