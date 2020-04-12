#!/bin/sh
cd $(dirname $0)/.. && elm-live --dir=./public --start-page=index.html src/Main.elm -- --output=public/main.js --debug
