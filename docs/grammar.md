# Context-Free Grammar

Start form: F

## File

F -> IP

## Imports

I -> EMPTY

I -> `Import` `Str` I

## Program

P -> EMPTY

P -> EP

## Expression

**Atoms**

E -> `Num`

E -> `Char`

E -> `Str`

E -> `Name`

E -> `True`

E -> `False`

E -> Void -> `Lparen` `Rparen`

**Arith**

E -> Neg -> `Sub` E

E -> Pos -> `Pos` E

E -> Add -> E `Add` E

E -> Sub -> E `Sub` E

E -> Mul -> E `Mul` E

E -> Div -> E `Div` E

E -> Mod -> E `Mod` E

**Logic**

E -> Not -> `Not` E

E -> And -> E `And` E

E -> Or -> E `Or` E

E -> Xor -> E `Xor` E

**Comp**

E -> Eq -> E `Eq` E

E -> Neq -> E `Neq` E

E -> Gt -> E `Gt` E

E -> Ge -> E `Ge` E

E -> Lt -> E `Lt` E

E -> Le -> E `Le` E

**Misc**

E -> List -> `Lbrack` Es `Rbrack`

E -> At -> E `Lbrack` E `Rbrack`

E -> FnVal -> `Fn` `Lparen` Params `Rparen` T E

E -> FnCall -> E `Lparen` Es `Rparen`

E -> `Lparen` E `Rparen`

E -> Bind -> `Bind` `Str` E

E -> If -> `If` E E `Else` E

E -> Block -> `Lbrace` P `Rbrace`

**Multiple Expressions**

Es -> Empty

Es -> E

Es -> E `Comma` Es

**Params**

Param -> `Name` T

Params -> EMPTY

Params -> Param

Params -> Param `Comma` Params

## types

T -> `Name`

T -> `Lparen` T `Rparen`

T -> List -> T `Lbrack` `Rbrack`

T -> Fn -> T `Lparen` Ts `Rparen`

Ts -> EMPTY

Ts -> T

Ts -> T `Comma` Ts
