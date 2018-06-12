#!/usr/bin/env bash
cd root
files=$(find . -mindepth 1 -maxdepth 1)
for f in $files; do
    bn=$(basename $f)
    ln -s $(pwd)/$bn ~/.$bn
done

# zsh {{
# grml zsh config
wget -O ~/.zshrc https://git.grml.org/f/grml-etc-core/etc/zsh/zshrc

# install zsh scripts
script_dir=~/.local/share
mkdir -p $script_dir
cd $script_dir

# history substring search
git clone git@github.com:zsh-users/zsh-history-substring-search.git
# }}
