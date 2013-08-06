for f in $(git ls-files -u | cut -f 2 | sort -u)
do
    echo $f
    git co --theirs "$f"
    git add "$f"
done

git ci
