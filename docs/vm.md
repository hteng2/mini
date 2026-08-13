# VM specification

## Machine

Data is stored in:

1. the bytecode buffer containing code

2. the storage stack, containing variables

3. the operation stack, used for evaluating expressions

4. the jump list, for returning from functions

### Bytecode buffer

Fixed-size, read-only buffer containing the bytecode, beginning with a 8-byte header containing the symbol count.

### Storage stack

Size-extendable stack used for storing variables. Each new stack frame corresponds to a new section of this stack.

New frames are created for each function call (not tail calls).

Values are aligned to 8 bytes.

### Operation stack

The operation stack is a LIFO stack used for evaluating expressions.

Values are aligned to 8 bytes.

In a function call, the program counter (to return to), function parameters, and closure values are pushed onto the operation stack, then stored by the function body.

### Jump List

Storing absolute locations in the bytecode to jump to when returning from a function.

Pushed in a function call (not tailcalls).

## Bytecode

The bytecode buffer holds the header followed by a sequence of instructions.

### Instruction format

Each instruction consists of two bytes — an **opcode** byte and a **sub-opcode** byte — followed by an optional operand:

- the opcode selects the instruction,
- the sub-opcode selects a variant of the instruction or carries a small immediate value,
- the operand, when present, is an 8-byte little-endian integer.

Floats are encoded as their IEEE-754 bit patterns. Instructions without an operand are 2 bytes long; operand-bearing instructions are 10 bytes long, except for `fn`, which is variable-length (see below).

### Instruction set

| Opcode | Sub-opcode | Instruction | Operand       | Description                                              |
| ------ | ---------- | ----------- | ------------- | -------------------------------------------------------- |
| 0      | 0          | `pushi`     | int           | push an integer                                          |
| 0      | 1          | `pushf`     | float         | push a float                                             |
| 1      | v          | `pushc`     | –             | push the small value `v`                                 |
| 2      | 0          | `pop`       | –             | discard the top of the operation stack                   |
| 3      | n          | `op`        | –             | arithmetic, comparison, or logic operation (see below)   |
| 4      | 0          | `list`      | int           | pop `n` values and push a list of them (LIFO)            |
| 5      | 0          | `at`        | –             | pop an index then a list, push the element at that index |
| 6      | 0          | `tuple`     | int           | pop `n` values and push a tuple of them (LIFO)           |
| 7      | 0          | `destruct`  | –             | pop a tuple and push its elements (FIFO)                 |
| 8      | 0          | `fn`        | **see below** | create a closure                                         |
| 9      | 0          | `call`      | –             | call a closure or builtin                                |
| 9      | 1          | `tailcall`  | –             | tail-call a closure or builtin                           |
| 10     | 0          | `bind`      | int           | pop a value and store it in variable slot `n`            |
| 10     | 1          | `val`       | int           | push the value of variable slot `n`                      |
| 11     | 0          | `if`        | –             | pop a condition; skip the next instruction if true       |
| 12     | 0          | `jmp`       | int           | add the operand to the program counter                   |
| 13     | 0          | `jmpbck`    | –             | pops from the jumplist and absolute-jumps to it          |

Notes:

- `pushc` encodes its value in the sub-opcode byte: a character code, `1` for `true`, or `0` for `false` and `void`.
- `jmpbck` returns from a function body by jumping to the address saved by the most recent `call`/`tailcall`.
- `bind` and `val` address variable slots of the current frame. The header's symbol count is the size of the initial frame, whose first slots hold the builtins.

### Operations (opcode 3)

The sub-opcode selects the operation:

| Sub-opcode | Operation | Operand types |
| ---------- | --------- | ------------- |
| 0          | `ineg`    | int           |
| 1          | `iadd`    | int, int      |
| 2          | `isub`    | int, int      |
| 3          | `imul`    | int, int      |
| 4          | `idiv`    | int, int      |
| 5          | `imod`    | int, int      |
| 8          | `ieq`     | int, int      |
| 9          | `ineq`    | int, int      |
| 10         | `ilt`     | int, int      |
| 11         | `ile`     | int, int      |
| 12         | `igt`     | int, int      |
| 13         | `ige`     | int, int      |
| 16         | `fneg`    | float         |
| 17         | `fadd`    | float, float  |
| 18         | `fsub`    | float, float  |
| 19         | `fmul`    | float, float  |
| 20         | `fdiv`    | float, float  |
| 26         | `fgt`     | float, float  |
| 28         | `flt`     | float, float  |
| 32         | `not`     | bool          |
| 33         | `and`     | bool, bool    |
| 34         | `or`      | bool, bool    |
| 35         | `xor`     | bool, bool    |

Unary operations (`ineg`, `fneg`, `not`) pop one operand and push the result; binary operations pop two operands (the right one first) and push one result. Comparisons and logic operations push `1` or `0` as an integer. The operation does not carry the operand type — int, char, and bool comparisons share sub-opcodes 8–13.

### Closures (opcode 8)

The `fn` instruction is followed by its operands, each 8 bytes little-endian:

1. the number of captured variables, `n`,
2. `n` variable slots — the indices, in the current frame, of the captured variables,
3. the symbol count — the number of variable slots in the closure's frame,
4. the body size — the number of instructions making up the function body.

Executing `fn` captures the listed variables into a closure value, records the address of the instruction after the operands (the start of the body), and skips over the body.
