# tokens definition

| token              | regex                  |
| ------------------ | ---------------------- |
| values             |                        |
| `Num`              | `[0-9][0-9]*`          |
| `Str`              | `"(\[^\\\]\|\\.)*"`    |
| `Char`             | `'(\[^\\\]\|\\.)'`     |
| keywords           |                        |
| `True`             | `true`                 |
| `False`            | `false`                |
| `Void`             | `_`                    |
| `Let`              | `let`                  |
| `Var`              | `var`                  |
| `Print`            | `print`                |
| `Println`          | `println`              |
| `If`               | `if`                   |
| `Else`             | `else`                 |
| `While`            | `while`                |
| `Break`            | `break`                |
| `Continue`         | `continue`             |
| `Fn`               | `fn`                   |
| `Return`           | `return`               |
| `Name`             | `[a-zA-Z][a-zA-Z0-9]*` |
| operators          |                        |
| `Lparen`           | `(`                    |
| `Rparen`           | `)`                    |
| `Lbrack`           | `[`                    |
| `Rbrack`           | `]`                    |
| `Lbrace`           | `{`                    |
| `Rbrace`           | `}`                    |
| `Comma`            | `,`                    |
| ordered comparison |                        |
| `Eq`               | `=`                    |
| `Gt`               | `>`                    |
| `Lt`               | `<`                    |
| arithmetic         |                        |
| `Add`              | `+`                    |
| `Sub`              | `-`                    |
| `Mul`              | `*`                    |
| `Div`              | `/`                    |
| `Mod`              | `%`                    |
| logic              |                        |
| `And`              | `&`                    |
| `Or`               | `\|`                   |
| `Not`              | `!`                    |
| `Xor`              | `^`                    |
