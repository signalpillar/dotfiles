#!/bin/bash

control_c()
# run if user hits control-c
{
  echo -en "\n*** Ouch! Exiting ***\n"
  exit $?
}

# trap keyboard interrupt (control-c)
#trap control_c SIGINT


CURRENT_HASHES=/tmp/diff.hash

PROCESSED_HASHES=/tmp/diff.processed.hash

# save current hashes
# 1) author
# 2) range CP..master
git log --reverse --format=%H --author=$1 $2 > $CURRENT_HASHES

declare LINES=($(cat $CURRENT_HASHES | grep -vf $PROCESSED_HASHES))

for l in ${LINES[@]}
do
    # save into processed hashes
    #echo $l >> $PROCESSED_HASHES
    git show --name-only $l
    read -p "Process ? [yn] " -n 1
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Skipped"
    else
        echo 'passed'
        git cherry-pick $l
        errcode=$?
        if [ $errcode -eq 0 ]; then
            echo $l
        else
            echo "failed $l"
            ~/bin/resolve-theirs.sh
        fi
    fi
    if [[ $REPLY =~ ^[YyNn]$ ]]; then
    echo $l >> $PROCESSED_HASHES
    fi
done
