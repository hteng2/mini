# VM specification

## Machine

Data is stored in:

1. the bytecode buffer, containing code and constant pools

2. the storage stack, containing variables

3. the operation stack, used for evaluating expressions

4. runtime registers, used for evaluating expression

### Bytecode buffer

Fixed-size, read-only buffer containing the bytecode and constant pools.

Constant pools precede the bytecode that accesses it and provides runtime values that do not fit in an instruction (large integers, strings, lists, closures, etc.).

Constant pools are 8-byte aligned, bytecode is 4-byte aligned.

There is one global constant pool, as well as one local constant pool per function.

Bytecode and constant pool sections contain a 8-byte header that specifies the section's size (top 62 bits) and type (bottom 2 bits).

The entire buffer also has a 8-byte header that specifies the starting program counter value.

**Format**:

```
starting pc value (8-byte)

global constant pool:
header: size (62 bits), type = 1 (2 bits)

global bytecode:
header: size (62 bits), type = 0 (2 bits)

function 1:
local constant pool:
header: size (62 bits), type = 1 (2 bits)
if AT

local bytecode:
header: size (62 bits), type = 0 (2 bits)

function 2:
...

```

### Storage stack

The storage stack is a size-extendable stack used for storing variables. Each new stack frame corresponds to a new section of this stack.

All sections start aligned to 8 bytes.

Values are aligned to their size (8 bytes or 1 byte)

### Operation stack

The operation stack is a LIFO stack used for evaluating expressions.

Values are aligned to their size (8 bytes or 1 byte)

In a function call, the program counter (to return to), function parameters, and closure values are pushed onto the operation stack, then stored by the function body.

### Registers

Special registers (read only):

0. n (**n**ull) does nothing, always 0

1. t (**t**ime) number of steps so far

2. bp (**b**ase **p**ointer) points to the beginning of the current stack frame in the storage-stack

3. lp (**l**iteral **p**ointer) points to the starting location of the active literal pool

4. pc (**p**rogram **c**ounter) points to the current instruction in the bytecode

5. ec (**e**rror **c**ode) holds the error code of the last operation

Result registers (read only):

6. Result register 1 (first result of an operation / return value of function)

7. Result register 2 (second result of an operation)

Normal registers (read/write):

8-15. anything

## Bytecode

Codes are fixed to be 32 bits

0-3: code
4-7: flags
8-31: parameters

**Flags**

Data Size (DZ): 1 bit

determines the size of an operation

0: 1 byte

1: 8 bytes

Data Type (DT): 2 bits

determines the type of an operation

0: bool

1: char

2: addr/int

3: float

Arithmetic Type (AT): 2 bits

0: add

1: sub

2: mul

3: div/mod

Comparison Type (CT): 2 bits

0: eq

1: gt

2: lt

Logic Type (LT): 2 bits

0: and

1: or

2: not

3: xor

Move Type (MF): 3 bits

0: o-stack -> register

1: register -> o-stack

2: s-stack -> register

3: register -> s-stack

4: o-stack -> s-stack

5: s-stack -> o-stack

6: buffer -> o-stack

7: buffer -> s-stack

### Exit (0)

0. **Exit**

flags: none

params: none

result: exits the program

### Moving Data (1-7)

1. **Move**

flags: MF

params: size register #, src register #, dst register #

result: copies size bytes, with src and dst as locations

2. **Copy**

flags: none

params: src register #, dst register #

result: copies values from src to dst

3. **Put**

flags: none

params: register #, value (2 bytes)

result: puts value in register

4. **Save**

flags: none

params: none

result: saves all registers in the

### Control flow (8-11)

8. **Ifn**

flags: none

params: condition register #

result: if condition is false, pc skips the next instruction

9. **Jmp**

flags: none

params: offset register #

result: pc jumps by offset

10. **Goto**

flags: none

params: location register #

result: pc jumps to location

11. **Call**

flags: none

params: builtin #

result: calls the builtin with that number, result depends on builtin

### Operations (12-15)

12. **Arithmetic**

flags: DT, AT

params: register a, register b, register c

result: performs operation a and b, puts result in c

13. **Comparison**

flags: DT, CT

params: register a, register b, register c

result: performs comparison a and b, puts result in c

14. **Logic**

flags: LT

params: register a, register b, register c

result: performs logic operation a and b, puts result in c

15. **Address**

flags: DZ

params: register a, register b, register c

result: a + size * b -> c

## Representation

### Functions

The body of the function is stored in its own section in the bytecode buffer.

The closure of the function is stored in the storage stack.

Format for closure: 8-byte function location, closure size in bytes, closure values

Function calls become:

1. Push pc + 4 to stack (return to next instruction)
2. Push params to stack
3. Push closure values to stack
4. Jump to function location

### Lists

Lists with known values at compile time are stored in the bytecode buffer.

Lists created dynamically are stored in the storage stack

Lists always contain an 8-byte header storing its size in bytes

Index lists using `8 + list location + data size * index`

### Tuples (once implemented)

Tuples are stored in the storage stack.

Format for tuple: tuple size in bytes, tuple values
