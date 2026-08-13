# Types

int | float | bool | char | void | list (T) | fn (T, T) | tuple (T[])

## Inference rules

**Atoms**

Num: int

Char: char

Str: list char

Name: ?

Bool: bool

Void: void

**Arith**

Neg: int -> int | float -> float

Pos: int -> int | float -> float

Add: int, int -> int | float, float -> float

Sub: int, int -> int | float, float -> float

Mul: int, int -> int | float, float -> float

Div: int, int -> int | float, float -> float

Mod: int, int -> int

**Logic**

Not: bool -> bool

And: bool, bool -> bool

Or: bool, bool -> bool

Xor: bool, bool -> bool

**Comp**

Eq: int, int -> bool | bool, bool -> bool | char, char -> bool

Neq: int, int -> bool | bool, bool -> bool | char, char -> bool

Gt: int, int -> bool | float, float -> bool | char, char -> bool

Ge: int, int -> bool | char, char -> bool

Lt: int, int -> bool | float, float -> bool | char, char -> bool

Le: int, int -> bool | char, char -> bool

**Misc**

At: list T, int -> T

FnCall: fn (T1, T2) -> T1 -> T2

Bind: T -> Void

If: bool, T, T -> T

Block: ..., T -> T
