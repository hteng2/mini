# tokens definition

| token       | regex                     |
| ----------- | ------------------------- |
| values      |                           |
| `Int`       | `[0-9][0-9]*`             |
| `Float`     | `[0-9][0-9]*\\\.[0-9]*`   |
| `Str`       | `"(\[^\\\\\]\|\\\\\\.)*"` |
| `Char`      | `'(\[^\\\\\]\|\\\\\\.)'`  |
| keywords    |                           |
| `True`      | `true`                    |
| `False`     | `false`                   |
| `Void`      | `_`                       |
| `Bind`      | `bind`                    |
| `If`        | `if`                      |
| `Else`      | `else`                    |
| `While`     | `while`                   |
| `Break`     | `break`                   |
| `Continue`  | `continue`                |
| `Fn`        | `fn`                      |
| `Name`      | `[a-zA-Z][a-zA-Z0-9]*`    |
| operators   |                           |
| `Lparen`    | `(`                       |
| `Rparen`    | `)`                       |
| `Lbrack`    | `[`                       |
| `Rbrack`    | `]`                       |
| `Lbrace`    | `{`                       |
| `Rbrace`    | `}`                       |
| `Comma`     | `,`                       |
| `Semicolon` | `;`                       |
| comparison  |                           |
| `Eq`        | `=`                       |
| `Neq`       | `!=`                      |
| `Gt`        | `>`                       |
| `Ge`        | `>=`                      |
| `Lt`        | `<`                       |
| `Le`        | `<=`                      |
| arithmetic  |                           |
| `Add`       | `+`                       |
| `Sub`       | `-`                       |
| `Mul`       | `*`                       |
| `Div`       | `/`                       |
| `Mod`       | `%`                       |
| logic       |                           |
| `And`       | `&`                       |
| `Or`        | `\|`                      |
| `Not`       | `!`                       |
| `Xor`       | `^`                       |
