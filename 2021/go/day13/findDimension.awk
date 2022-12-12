#! /usr/bin/awk -f

{
    if (!($0 ~ /fold.*/)) {
        split($0,a,",");
        if (a[1] > maxI) {
            maxI = a[1]
        }
        if (a[2] > maxJ) {
            maxJ = a[2]
        }
    }
} END {
    print "Dimension are " maxI+1 "x" maxJ+1
}
