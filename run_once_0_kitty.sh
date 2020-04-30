# We need to run this to let tmux know what is kitty terminal
KITTY_TERMINFO_DIR=/Applications/kitty.app/Contents/Frameworks/kitty/terminfo
if [ -d $KITTY_TERMINFO_DIR ]; then
    ln -Fs $KITTY_TERMINFO_DIR  ~/.terminfo
fi
