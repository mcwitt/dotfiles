#!/usr/bin/env bash
cd root
files=$(find . -mindepth 1 -maxdepth 1)
for f in $files; do
    rm -r ~/.$(basename $f)
done
