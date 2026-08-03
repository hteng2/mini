# grammar definition

Start form: F

## file

F ::= IP

## imports

I ::= EMPTY | `Import` `Str` I

## program

P ::= EMPTY | EP

## expressions

E ::=

| `Num`

| `Char`

| `Str`

| `Name`

| `True`

| `False`

| `Lparen` `Rparen`

| `Sub` E

| `Pos` E

| E `Add` E

| E `Sub` E

| E `Mul` E

| E `Div` E

| E `Mod` E

| `Not` E

| E `And` E

| E `Or` E

| E `Xor` E

| E `Eq` E

| E `Neq` E

| E `Gt` E

| E `Ge` E

| E `Lt` E

| E `Le` E

| `Lbrack` Es `Rbrack`

| E `Lbrack` E `Rbrack`

| `Fn` `Lparen` Ps `Rparen` type E

| E `Lparen` Es `Rparen`

| `Lparen` E `Rparen`

| `Let` `Str` E

| `If` E E `Else` E

| `While` E E

| `Break`

| `Continue`

| `Lbrace` P `Rbrace`

Es ::= EMPTY | E `Comma` Es

Ps ::= EMPTY | `Name` T `Comma` Ps

## types

T ::=

| `Name`

| `Lparen` T `Rparen`

| T `Lbrack` `Rbrack`

| T `Lparen` Ts `Rparen`

Ts ::= EMPTY | T `Comma` Ts
