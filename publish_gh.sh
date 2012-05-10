#!/bin/bash

sh build.sh
rm -rf temp
mkdir temp
cp prelude.* temp
rm prelude.*
git checkout gh-pages
cp temp/prelude.* .
cat header.html prelude.raw.html footer.html > index.html
