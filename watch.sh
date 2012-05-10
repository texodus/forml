#!/bin/bash

sh build.sh

for (( ; ; ))
do
    if [ src/formal/prelude.formal -nt prelude.js ]
    then 
        sh build.sh
        sleep 3
    fi
    sleep 3
done
