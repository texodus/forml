#!/bin/bash


for (( ; ; ))
do
    if [ src/sonnet/prelude.formal -nt prelude.js ]
    then 
        cabal build
        ./dist/build/sonnet/sonnet src/sonnet/prelude.formal
        java -jar ./lib/compiler.jar --js prelude.js --js_output_file prelude2.js --formatting=pretty_print
        mv prelude2.js prelude.js
    fi
    sleep 3
done
