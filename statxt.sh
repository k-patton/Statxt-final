#!/bin/bash

LLI="lli"

LLC="llc"

CC="cc"
STATXT="./statxt.native"

Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

basename=`echo $1 | sed 's/.*\\///
                         s/.stxt//'`

touch temp.stxt
cat stdlib.stxt > temp.stxt
cat $1 >> temp.stxt

Run "$STATXT" "temp.stxt" ">" "temp.ll" &&
Run "$LLC" "temp.ll" ">" "temp.s" &&
Run "$CC" "-o" "temp.exe" "temp.s" "cfunctions.o" &&
Run "./temp.exe"
