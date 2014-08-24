lccyao
======

the LCCyao compiler

This project was written by Benjamin Kreuter (brkb7x@virginia.edu), Chih-Hao Shen (), and Benjamin Terner (bt3ze@virginia.edu) and is currently upkept by Benjamin Terner and Benjamin Kreuter.

Usage

Evaluating functions securely takes a number of steps; the following guide should help:

1. install LCC (the Little C Compiler) on your system
2. use lcc-bytecode-gen.sh, or equivalently, compile your C program using the format
   lcc -c -S -target=bytecode program.c o- program.lcc
3. convert your program in LCC bytecode to a program in PCF2 bytecode, using translate.sh
   ./translate.sh program.lcc program.pcf2
4. run your PCF2 program or generate gates. to simulate execution, use simulate.sh
   ./simulate.sh program.pcf2 inputfile.txt
PCF2 programs can be used to simply generate gates using cirgen, which is found in pcflib/.  
PCF2 programs may also be evaluated using our gen and eval tools secure 2-party computation. this requires making those programs, and a good guide is not yet available (please email us)