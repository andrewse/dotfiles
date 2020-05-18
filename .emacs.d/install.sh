#!/usr/bin/env bash
EMACSDIR=$HOME/.emacs.d

if ! [ -d "${EMACSDIR}" ]; then
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
fi

function copy-dotfile() {
    echo Copying $1
    cp -r ${BASH_SOURCE%/*}/$1 $HOME/
}

# copy-dotfile .emacs
copy-dotfile .spacemacs

# From https://github.com/syl20bnr/spacemacs/issues/11801
find ~/.emacs.d/elpa/org*/*.elc -print0 | xargs -0 rm
find ~/.emacs.d/.cache/*.elc -print0 | xargs -0 rm

rsync -avP ${BASH_SOURCE%/*}/.emacs.d/ $EMACSDIR
