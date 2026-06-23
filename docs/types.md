# type spec definition

types = int | bool | char | string | void

ordered = int | bool

Num : int

Char : char

Str : string

Name : types

True : bool

False : bool

Neg : int -> int

Pos : int -> int

Not : bool -> bool

Eq : ordered * ordered -> bool

Gt : int * int -> bool

Lt : int * int -> bool

Add : int * int -> int

Sub : int * int -> int

Mul : int * int -> int

Div : int * int -> int

And : bool * bool -> bool

Or : bool * bool -> bool

Xor : bool * bool -> bool

Let:

let <name> = type

If :

if bool then ... end

| if bool then ... else ... end
