#!/usr/bin/env bash
cd root
files=$(find . -depth 1 | xargs basename)
for f in $files; do
    ln -s $(pwd)/$f ~/.$f
done
