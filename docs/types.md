# Types

T -> int

T -> float

T -> bool

T -> char

T -> void

T -> list T

T -> fn (T, T)

T -> tuple T []

## Expressions

**Atoms**

Num: int

Char: char

Str: list char

Name: ?

Bool: bool

Void: void

**Arith**

Neg: int -> int

Neg: float -> float

Pos: int -> int

Pos: float -> float

Add: int, int -> int

Add: float, float -> float

Sub: int, int -> int

Sub: float, float -> float

Mul: int, int -> int

Mul: float, float -> float

Div: int, int -> int

Div: float, float -> float

Mod: int, int -> int

**Logic**

Not: bool -> bool

And: bool, bool -> bool

Or: bool, bool -> bool

Xor: bool, bool -> bool

**Comp**

Eq: int, int -> bool

Eq: bool, bool -> bool

Eq: char, char -> bool

Neq: int, int -> bool

Neq: bool, bool -> bool

Neq: char, char -> bool

Gt: int, int -> bool

Gt: float, float -> bool

Gt: char, char -> bool

Ge: int, int -> bool

Ge: float, float -> bool

Ge: char, char -> bool

Lt: int, int -> bool

Lt: float, float -> bool

Lt: char, char -> bool

Le: int, int -> bool

Le: float, float -> bool

Le: char, char -> bool

**Misc**

List: T, ... -> list T

At: list T, int -> T

FnVal: fn T1, T2

FnCall: fn T1, T2 -> T1 -> T2

Bind: T -> Void

If: bool, T, T -> T

Block: ..., T -> T
