#!/bin/bash

# Script borrowed from:  https://blog.rubenwardy.com/2018/05/07/mingw-copy-dlls/

BINDIR="./bin"
EXE="$BINDIR/new_fdm.exe"

paths=("/mingw64/bin"
    "/mingw64/lib")

function findAndCopyDLL() {
    for i in "${paths[@]}"
    do
        FILE="$i/$1"
        if [ -f $FILE ]; then
           cp $FILE $BINDIR
           echo "Found $1 in $i"
           copyForOBJ $FILE
           return 0
        fi
    done

    return 1
}

function copyForOBJ() {
    dlls=`objdump -p $1 | grep 'DLL Name:' | sed -e "s/\t*DLL Name: //g"`
    while read -r filename; do
        findAndCopyDLL $filename || echo "Unable to find $filename"
    done <<< "$dlls"
}

copyForOBJ $EXE
