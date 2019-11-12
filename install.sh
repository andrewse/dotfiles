#!/bin/bash
echo Installing dotfiles to $HOME
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
RELATIVE_HOME=$(realpath --relative-to=${DIR} ${HOME})

for directory in {bash,git,emacs}; do
    stow -vv --target=${RELATIVE_HOME} "${directory}"
done;
unset directory
