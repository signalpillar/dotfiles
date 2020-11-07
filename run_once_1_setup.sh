#!/bin/bash

touch ~/.env.sh
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

function setup_emacs {
    git clone https://github.com/syl20bnr/spacemacs $HOME/.emacs.d
    cd $HOME/.emacs.d
    git checkout develop
    git pull
}
