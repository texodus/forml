#!/bin/bash

cabal build && ./dist/build/formal/formal -t src/formal/prelude.formal
