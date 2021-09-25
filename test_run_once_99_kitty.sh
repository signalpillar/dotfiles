#!/bin/bash

# We need to run this to let tmux know what is kitty terminal
KITTY_TERMINFO_DIR=/Applications/kitty.app/Contents/Frameworks/kitty/terminfo
if [ -d $KITTY_TERMINFO_DIR ]; then
    ln -Fs $KITTY_TERMINFO_DIR  ~/.terminfo
fi


THEMES_DIR=~/.config/kitty/kitty-themes
if [ ! -d $THEMES_DIR ]; then
    git clone --depth 1 git@github.com:dexpota/kitty-themes.git $THEMES_DIR
fi


ln -Fs $THEMES_DIR/themes/CLRS.conf ~/.config/kitty/theme.conf


