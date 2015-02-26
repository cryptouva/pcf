PCF
======

This repo contains the LCCyao compiler and BetterYao interpreter for Portable Circuit Format. 
See the USENIX paper: https://www.usenix.org/conference/usenixsecurity13/technical-sessions/paper/kreuter

This project was written by Benjamin Kreuter (brkb7x@virginia.edu), Chih-Hao Shen (), and Benjamin Terner (bt3ze@virginia.edu) and is currently upkept by Benjamin Terner and Benjamin Kreuter. Please contact Ben Terner with questions or issues.


Usage

Evaluating functions securely takes a number of steps; the following guide should help:

1. install LCC (the Little C Compiler: https://sites.google.com/site/lccretargetablecompiler/)
2. use lcc-bytecode-gen.sh, or equivalently, compile your C program using the format
   lcc -c -S -target=bytecode program.c -o program.lcc
3. convert your program in LCC bytecode to a program in PCF2 bytecode, using translate.sh
   ./translate.sh program.lcc program.pcf2
4. run your PCF2 program or generate gates. to simulate execution, use simulate.sh
   ./simulate.sh program.pcf2 inputfile.txt
PCF2 programs can be used to simply generate gates using cirgen, which is found in pcflib/.  
PCF2 programs may also be evaluated using our betteryao tools for secure 2-party computation. this requires making those programs, and a guide is available at https://drive.google.com/open?id=1kwk-t-GSbqWivhu4WbgSZVNSmpO4yA6S5ntIx9LzgOE&authuser=1 
