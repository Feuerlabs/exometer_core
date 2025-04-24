#!/bin/sh
set -e

APP=$(basename "$PWD")
SRC="_build/default/lib/$APP"
DST="$PWD/_build/zomp/lib/$APP"
mkdir -p "$DST"
find "$SRC" -type l ! -exec test -e {} \; -delete
cp -aR -L "$SRC/." "$DST/"
cp "$PWD/zomp.meta" "$DST/"
rm "$DST"/ebin/*.beam
