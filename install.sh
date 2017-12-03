#!/usr/bin/env bash
cd root
files=$(find . -mindepth 1 -maxdepth 1)
for f in $files; do
    bn=$(basename $f)
    ln -s $(pwd)/$bn ~/.$bn
done
