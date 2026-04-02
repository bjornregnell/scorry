#!/bin/bash
set -e

DEST=../bjornregnell.github.io/scorry
SBT_OUT=target/scala-3.8.3/scorry-opt

echo "Building optimised JS..."

sbt --client clean
sbt --client fullOptJS

mkdir -p "$DEST"

echo "Copying files to $DEST..."
cp "$SBT_OUT/main.js"     "$DEST/main.js"
cp "$SBT_OUT/main.js.map" "$DEST/main.js.map"
sed 's|./target/scala-3.8.3/scorry-fastopt.js|main.js|' index.html > "$DEST/index.html"

echo "Visit $DEST and commit and push to finish publishing"
