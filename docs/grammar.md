# grammar definition

Start form: P

## program

P ::= EMPTY | DP

## declarations

D ::= Let | Var | VarSet | Print | If | While

Let ::= `Let` `Name` `Eq` E

Var ::= `Var` `Name` `Eq` E

VarSet ::=

| `Name` `Eq` E

| `Name` `Lbrack` E `Rbrack` `Eq` E

Print ::= `Print` E

If ::=

| `If` E `Then` P `End`

| `If` E `Then` P `Else` P `End` (this is unimplemented)

While ::= `While` E `Do` P `Done`

## expressions

E ::=

| `Num`

| V

| `True`

| `False`

| `Sub` E

| `Pos` E

| `Not` E

| E `Eq` E

| E `Gt` E

| E `Lt` E

| E `Add` E

| E `Sub` E

| E `Mul` E

| E `Div` E

| E `And` E

| E `Or` E

| E `Xor` E

| `Lparen` E `Rparen`

| `Lbrack` Es `Rbrack`

V ::=

| `Name`

| Var `Lbrack` E `Rbrack`

Es ::= E | E `Comma` Es
