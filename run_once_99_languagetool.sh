#!/bin/bash

# https://languagetool.org/
# For emacs integration with langauge tool we need to download it first.
# https://github.com/jsmestad/spacemacs-langtool


VERSION=4.9.1
DIR_NAME="LanguageTool-$VERSION"
ARCHIVE="LanguageTool-$VERSION.zip"
TARGET_DIR="$HOME/languagetool"
URL="https://languagetool.org/download/$ARCHIVE"

if ! [ -d $TARGET_DIR ]; then
    mkdir -p $TARGET_DIR
    tmp_dir=/tmp
    cd $tmp_dir
    unarchived_dir="$tmp_dir/$DIR_NAME"
    if ! [ -d $unarchived_dir ]; then
        echo "Download $URL"
        wget $URL
        unzip $ARCHIVE
    fi
    cp -R $unarchived_dir/* $TARGET_DIR
fi

SPACEMACS_LAYER="$HOME/.emacs.d/private/languagetool"
if ! [ -d $SPACEMACS_LAYER ]; then
    git clone https://github.com/jsmestad/spacemacs-langtool.git $SPACEMACS_LAYER
fi
