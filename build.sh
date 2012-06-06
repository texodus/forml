#!/bin/bash

function clean {
# java -jar ./lib/compiler.jar --js prelude.js --js_output_file prelude2.js --formatting=pretty_print --compilation_level ADVANCED_OPTIMIZATIONS --warning_level QUIET
cat lib/js/jasmine-1.0.1/jasmine.js prelude.js prelude.spec.js lib/js/console.js > prelude.node.js
cat header.html prelude.html footer.html > prelude2.html
mv prelude2.html prelude.html
node prelude.node.js

}

cabal build && ./dist/build/formal/formal -t src/formal/prelude.formal && clean
