# tokens definition

| token              | regex                     |
| ------------------ | ------------------------- |
| values             |                           |
| `Int`              | `[0-9][0-9]*`             |
| `Float`            | `[0-9][0-9]*\\\.[0-9]*`   |
| `Str`              | `"(\[^\\\\\]\|\\\\\\.)*"` |
| `Char`             | `'(\[^\\\\\]\|\\\\\\.)'`  |
| keywords           |                           |
| `True`             | `true`                    |
| `False`            | `false`                   |
| `Void`             | `_`                       |
| `Let`              | `let`                     |
| `Var`              | `var`                     |
| `Set`              | `set`                     |
| `If`               | `if`                      |
| `Else`             | `else`                    |
| `While`            | `while`                   |
| `Break`            | `break`                   |
| `Continue`         | `continue`                |
| `Fn`               | `fn`                      |
| `Name`             | `[a-zA-Z][a-zA-Z0-9]*`    |
| operators          |                           |
| `Lparen`           | `(`                       |
| `Rparen`           | `)`                       |
| `Lbrack`           | `[`                       |
| `Rbrack`           | `]`                       |
| `Lbrace`           | `{`                       |
| `Rbrace`           | `}`                       |
| `Comma`            | `,`                       |
| `Semicolon`        | `;`                       |
| `Set`              | `=`                       |
| ordered comparison |                           |
| `Eq`               | `==`                      |
| `Neq`              | `!=`                      |
| `Gt`               | `>`                       |
| `Ge`               | `>=`                      |
| `Lt`               | `<`                       |
| `Le`               | `<=`                      |
| arithmetic         |                           |
| `Add`              | `+`                       |
| `AddEq`            | `+=`                      |
| `Sub`              | `-`                       |
| `SubEq`            | `-=`                      |
| `Mul`              | `*`                       |
| `MulEq`            | `*=`                      |
| `Div`              | `/`                       |
| `DivEq`            | `/=`                      |
| `Mod`              | `%`                       |
| `ModEq`            | `%=`                      |
| logic              |                           |
| `And`              | `&`                       |
| `AndEq`            | `&=`                      |
| `Or`               | `\|`                      |
| `OrEq`             | `\|=`                     |
| `Not`              | `!`                       |
| `Xor`              | `^`                       |
| `XorEq`            | `^=`                      |
