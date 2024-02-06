#!/bin/bash

# Script borrowed from:  https://blog.rubenwardy.com/2018/05/07/mingw-copy-dlls/

DISTDIR="${PWD}/dist"
BINDIR="bin"
EXEDIR="${DISTDIR}/${BINDIR}"
EXE="${EXEDIR}/new_fdm.exe"
PREFIX="x86_64-w64-mingw32"

paths=("/usr/local/mingw64/bin"
    "/usr/local/mingw64/bin/x64"
    "/usr/$PREFIX/bin"
    "/usr/lib/gcc/$PREFIX/*-win32"
    "/usr/$PREFIX/lib")

function findAndCopyDLL() {
    for i in "${paths[@]}"
    do
        FILE="$i/$1"
        if [ -f $FILE ]; then
           cp $FILE $EXEDIR
           echo "Found $1 in $i"
           copyForOBJ $FILE
           return 0
        fi
    done

    return 1
}

function copyForOBJ() {
    dlls=`$PREFIX-objdump -p $1 | grep 'DLL Name:' | sed -e "s/\t*DLL Name: //g"`
    while read -r filename; do
        findAndCopyDLL $filename || echo "Unable to find $filename"
    done <<< "$dlls"
}

copyForOBJ $EXE
