#!/bin/bash

# Change to your local lcc installation directory
LCCDIR=$HOME/lib/lcc

tmpfilename=tmp$RANDOM

if [ $# -lt 1 ] 
  then
    echo "lcc_bytecode_gen.sh [.c input file]"
    exit
fi

$LCCDIR/gcc/cpp -U__GNUC__ -D_POSIX_SOURCE -D__STDC__=1 -D__STRICT_ANSI__ -Dunix -Di386 -Dlinux -D__unix__ -D__i386__ -D__linux__ -D__signed__=signed -I$LCCDIR/include -I$LCCDIR/gcc/include -I/usr/include $1 $tmpfilename.i 

$LCCDIR/rcc -target=bytecode $tmpfilename.i $1.lcc

rm $tmpfilename.i
