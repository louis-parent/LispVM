# LISP Virtual Machine

This project is part of the HAI705I lesson of compilation. The target is to write a compiler which convert Lisp code into a bytecode and a vm which can read this bytecode.
To avoid the syntax parsing, all the project is written in Lisp to let the Lisp interpreter parse the code into a tree structure.

The project is made in two parts : the compiler and the virtual machine.

## The compiler

The compiler converts a standard Common Lisp code into a bytecode with its own set of instruction.
The bytecode is formatted in a Lisp style with the format `(<instruction> [param1] [param2]...)`
The list of the instruction are the following :

| Instruction | Description |
| ----------- | ----------- |
| `(LOAD <src> <dest>)` | Loading from memory to register |
| `(STORE <src> <dest>)` | Storing from register to memory |
| `(MOVE <src> <dest>)` | Moving from register to register |
| `(ADD <src> <dest>)` | Addition |
| `(SUB <src> <dest>)` | Substraction |
| `(MUL <src> <dest>)` | Multiplication |
| `(DIV <src> <dest>)` | Division |
| `(INCR <dest>)` | Increment |
| `(DECR <dest>)` | Decrement |
| `(PUSH <src>)` | Storing to stack |
| `(POP <dest>)` | Loading from stack |
| `(LABEL <label>)` | Label creation |
| `(JMP <label>)` | Unconditional jump to label |
| `(JSR <label>)` | Jump with return (frame saving) |
| `(RTN)` | Return |
| `(CMP <src1> <src2>)` | Comparison between two values |
| `(JGT <label>)` | Jump to label if greater |
| `(JGE <label>)` | Jump to label if greater or equal |
| `(JLT <label>)` | Jump to label if less |
| `(JLE <label>)` | Jump to label if less or equal |
| `(JEQ <label>)` | Jump to label if equal |
| `(JNE <label>)` | Jump to label if not equal |
| `(TEST <src>)` | Comparison to NIL |
| `(JTRUE <label>)` | Jump to label if not NIL |
| `(JNIL <label>)` | Jump to label if NIL |
| `(NOP)` | Nothing |
| `(HALT)` | Stopping program |

## The virtual machine

The virtual machine (VM) reads the bytecode and execute it in a kind of simplified register machine.
The model used is the register machine to understand the functioning of a real machine.

The virtual machine is split in two parts : the memory and the register.

### The memory

The memory contains two blocks :
- The stack block is the traditional stack, which is used in order to save data more durably than the register
- The code block is the bytecode stored

### The register

The VM use a simplified model of 15 registers, which are the following :

| Register | Description |
| -------- | ----------- |
|    R0    | Generic data storing |
|    R1    | Generic data storing |
|    R2    | Generic data storing |
|    CP    | Code pointer (Beginning address of the code) |
|    PC    | Program counter (Current address in code) |
|    EC    | End code (Ending address of the code) |
|    BP    | Base pointer (Beginning address of the stack) |
|    SP    | Stack pointer (Next address of free space in the stack) |
|    ES    | End stack (Ending address of the stack) |
|    FP    | Frame pointer (Address of the stack splitting parameters and arguments of the current JSR) |
|   STOP   | Running machine flag (True if the machine is currently running) |
|   INF    | Inferior flag (True if the last comparison result is less) |
|   EQU    | Equal flag (True if the last comparison result is equal) |
|   SUP    | Superior flag (True if the last comparison result is greater) |
|   FNIL   | NIL flag (True if the last test result is NIL) |

## Examples

Some examples that can be compiled and run are placed in the examples' folder. The `*.asm` are already compile code that can be run with the VM, and `*.lisp` needs to be compiled before running.
