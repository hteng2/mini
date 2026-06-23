# grammar definition

Start form: F

## file

F ::= IP

## imports

I ::= EMPTY | `Import` `Str` I

## program

P ::= EMPTY | DP

## declarations

D ::= Let | Var | VarSet | Print | If | While | Break | Continue | Block | Return

Let ::= `Let` `Name` `Eq` E

Var ::= `Var` `Name` `Eq` E

VarSet ::=

| Id `Eq` E

Print ::= `Print` E

If ::=

| `If` E D

| `If` E D `Else` D

While ::= `While` E D

Break ::= `Break`

Continue ::= `Continue`

Block ::= `Lbrace` P `Rbrace`

Return ::= `Return` E

## expressions

E ::= Atom | Arith | Logic | Comp | List | Func | Group

Atom ::=

| `Num`

| `String`

| `Name`

| `True`

| `False`

| `Void`

Arith ::=

| `Sub` E

| `Pos` E

| E `Add` E

| E `Sub` E

| E `Mul` E

| E `Div` E

| E `Mod` E

Logic ::=

| `Not` E

| E `And` E

| E `Or` E

| E `Xor` E

Comp ::=

| E `Eq` E

| E `Gt` E

| E `Lt` E

List ::=

| `Lbrack` Es `Rbrack`

| E `Lbrack` E `Rbrack`

Func ::=

| `Fn` `Lparen` Ps `Rparen` type D

| E `Lparen` Es `Rparen`

Group ::=

| `Lparen` E `Rparen`

Es ::= EMPTY | E `Comma` Es

Ps ::= EMPTY | `Name` T `Comma` Ps

## types

T ::=

| `Name`

| T `Lbrack` `Rbrack`

| T `Lparen` Ts `Rparen`

Ts ::= EMPTY | T `Comma` Ts

## identifiers

Id ::=

| `Name`

| Id `Lbrack` E `Rbrack`
