#!/usr/bin/env bash
cd root
files=$(find . -mindepth 1 -maxdepth 1)
for f in $files; do
    bn=$(basename $f)
    ln -s $(pwd)/$bn ~/.$bn
done

# zsh {{
# grml zsh config
if [ ! -f "~/.zshrc" ]; then
    wget -O ~/.zshrc https://git.grml.org/f/grml-etc-core/etc/zsh/zshrc
fi

script_dir=~/.local/share
mkdir -p $script_dir
cd $script_dir
gh-clone() {
    if [ ! -d "$2" ]; then
        git clone git@github.com:$1/$2.git
    fi
}

gh-clone zsh-users zsh-history-substring-search
gh-clone zsh-users zsh-syntax-highlighting
# }}
