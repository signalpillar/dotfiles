#!/bin/bash

# We need to run this to let tmux know what is kitty terminal
KITTY_TERMINFO_DIR=/Applications/kitty.app/Contents/Frameworks/kitty/terminfo
if [ -d $KITTY_TERMINFO_DIR ]; then
    ln -Fs $KITTY_TERMINFO_DIR  ~/.terminfo
fi


if [ ! -d ~/.config/kitty/kitty-themes ]; then
    git clone --depth 1 https://github.com/dexpota/kitty-themes ~/.config/kitty/kitty-themes
fi

ln -Fs ~/.config/kitty/kitty-themes/themes/Github.conf ~/.config/kitty/theme.conf


