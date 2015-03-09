#The PCF2 Manual

This document contains an exlanation of the PCF2 bytecode format with some examples  
authors: bt3ze@virginia.edu, brkb7x@virginia.edu

#####A note about integer representation
while PCF2 is implemented independent of this decision, the LCCyao translator generally treats integers carried by wires as a list with the LSB given first and the MSB given last.

##PCF2 Bytecodes
 
###Instruction List:
 *  bits
 *  gate
 *  const
 *  add 
 *  sub 
 *  mul 
 *  initbase 
 *  clear
 *  copy 
 *  mkptr 
 *  copy-indir 
 *  indir-copy
 *  call
 *  ret
 *  branch
 *  label 
 *  join


###Explanations

####bits
splits an integer into its two's complement representation and stores it in the wires supplied

fields:

* dest -  the destination wires
* op1 - the source wire, usually holding a constant

####join
the (almost) inverse of _bits_, this combines an unsigned integer into a single constant value

fields:

* dest - the destination wire
* op1 - a list of wires given to join

####gate
emits a gate with given truth table, inputs, and output wire

fields:

 * dest - the destination wire
 * op1 - input wire #1
 * op2 - input wire #2
 * truth table - the given truth table to evaluate  
   For example, a few truth tables are given
     * AND #\*0001
     * OR #\*0111
     * XOR #\*0110
     * NOT #\*1100 (where op1 and op2 are supplied with the same gate)

####const
creates a constant value and stores it

fields:

 * dest - the destination wire
 * op1 - the value to store

####add, sub, mul
these three instructions perform arithmetic on input-independent values

fields:

 * dest - the destination wire
 * op1 - input wire #1
 * op2 - input wire #2

subtraction is performed as op1 - op2

####initbase
This instruction initializes the base pointer. It should only appear once, at the very beginning of the program.

fields:

* base - the location to set as base pointer

####clear
Set memory from baseptr to (baseptr + localsize) to 0. This is meant to be used at the entry to a function to clear size of the local variables

fields:

 * localsize - the amount of memory (number of wires) to wipe clean

####copy
Copies a value from one position to another, _x_=_y_

fields:

* dest - the first bit of the lhs (above, _x_)
* op1 - the first bit of the rhs (above, _y_)
* op2 - the number of wires to copy

####mkptr
Creates a pointer at a particular position

fields:

* dest - the position to convert into a pointer

####copy-indir 
Dereference a pointer and copy the dereferenced value to a location (x=*y)

fields:

 * dest - the location that will hold the copied values (above, _x_)
 * op1 - the wire holding an address to dereference (above, _y_)
 * op2 - the length to copy


####indir-copy 
Copy a value to the location pointed to by a pointer (*x=y)

fields:

 * dest - the address pointing to a location that will hold a copy (above, _x_)
 * op1 - the location to be copied (above, _y_)
 * op2 - the length to copy

For example, the following instructions are explained

    (CONST :DEST 227 :OP1 1 )                  load const 1 into 227
    (MKPTR :DEST 227 )                         turn address 227 into a pointer; it now references 1
    (CONST :DEST 228 :OP1 65 )                 load const 65 into 228
    (MKPTR :DEST 228 )                         turn address 228 into a pointer; it now references 65
    (COPY-INDIR :DEST 229 :OP1 228 :OP2 32 )   229-260 will now contain a copy of 65-96
    (INDIR-COPY :DEST 227 :OP1 229 :OP2 32 )   the location that 227 points to will contain a copy of 229-260

    With these 6 instructions, we copy 65-96 into locations 1-32.

####call
Calls a function given by _fname_ and sets the new base

fields:

* newbase - the new base pointer
* fname - the name of the function

####ret
Returns from a function call, setting the return value (if there is one)

fields:

 * value - the return value

**note: where is this placed **

####branch
Executes a conditional jump. Unconditional jumps can be implemented by putting a constant value into the condition wire

fields:

* cnd - the condition wire (0 = no go, 1 = go)
* targ - the string name of the target label to jump to

####label 
Assigns a label to an instruction address, used to name functions and bits of procedures delineated by the HLL compiler.

fields

* str - the name of the label
