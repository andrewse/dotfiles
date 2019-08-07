#!/bin/bash
echo Installing dotfiles to $HOME
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
RELATIVE_HOME=$(realpath --relative-to=${DIR} ${HOME})

stow -vv --target=${RELATIVE_HOME} bash
stow -vv --target=${RELATIVE_HOME} git
