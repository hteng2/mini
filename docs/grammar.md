# Context-Free Grammar

Start form: File

File -> Imports Program

Imports -> EMPTY | `Import` `Str` `Semicolon` Imports

Program -> EMPTY | Expr Program

## Expr (in order of priority)

Expr -> `Lparen` Expr `Rparen` | Atom | BiOp | UnOp | Stmt | Misc

**Atom**

Atom -> `Num` | `Char` | `Str` | `Name` | Bool | Void

Bool -> `True` | `False`

Void -> `Lparen` `Rparen`

**BiOp**

BiOp -> Expr operator Expr

When Expr between two operators, the one with higher power gets the Expr:

| operator                       | left | right |
| ------------------------------ | ---- | ----- |
| `Or`                           | 1    | 2     |
| `Xor`                          | 3    | 4     |
| `And`                          | 5    | 6     |
| `Eq` `Neq` `Gt` `Ge` `Lt` `Le` | 7    | 8     |
| `Add` `Sub`                    | 9    | 10    |
| `Mul` `Div` `Mod`              | 11   | 12    |

**UnOp**

UnOp -> `Add` Expr | `Sub` Expr | `Not` Expr

**Stmt**

Bind -> `Bind` Pattern(`Name`) Expr

If -> `If` `Lparen` Expr `Rparen` Expr `Else` Expr

**Misc**

List -> `Lbrack` Mult(Expr) `Rbrack`

At -> Expr `Lbrack` Expr `Rbrack`

Tuple -> `Lparen` Mult(Expr) `Rparen`

FnVal -> `Fn` `Lparen` Pattern(`Name` Type) `Rparen` Type Expr

FnCall -> Expr Expr

Block -> `Lbrace` Program `Rbrace`

## Mult(G)

Mult(G) -> EMPTY | G | G `Comma` Mult(G)

## Pattern(G)

Pattern(G) -> `Lparen` Pattern(G) `Rparen` | `Lparen` `Rparen` | G | `Lparen` Mult(Pattern(G)) `Rparen`

## Type

Type -> `Name` | Type `Lbrack` `Rbrack` | Type `To` Type | Pattern(Type)
