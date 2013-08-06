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
    echo $l >> $PROCESSED_HASHES
    git show --name-only $l
    read -p "Process ? [yn] " -n 1
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Skipped"
    else
        echo 'passed'
        git cherry-pick -n --keep-redundant-commits --allow-empty $l
        errcode=$?
        if [ $errcode -eq 0 ]; then
            echo $l
        else
            echo "failed $l"
            ~/bin/resolve-theirs.sh
        fi
    fi
done
