## epicus-doomicus-metallicus

`epicus-doomicus-metallicus` will be a very basic **scheme** interpreter written in `Scala`.

### changes

* v0.21 Added the `current-time-millis` primitive.
* v0.20 Added I/O primitives
    - `load`
    - `open-input-port`
    - `close-input-port`
    - `input-port?`
    - `eof-object?`
    - `read`
    - `read-char`
    - `peek-char`
    - `open-output-port`
    - `close-output-port`
    - `write-char`
    - `write`
    - `error`
* v0.19 Exposed the interaction environment. Creation of new environment is allowed.
        Support for the `eval` form
* v0.18 Support for the `apply` form
* v0.17 Added the `and` and `or` forms
* v0.16 Added the `let` form
* v0.15 Support for the `cond` form
* v0.14 Added support for the `begin` form
* v0.13 Added support for `lambda` functions
* v0.12 Added more primitive functions
    - `null?`
    - `boolean?`
    - `symbol?`
    - `integer?`
    - `char?`
    - `string?`
    - `pair?`
    - `procedure?`
    - `number->string`
    - `string->number`
    - `symbol->string`
    - `string->symbol`
    - `char->integer`
    - `integer->char`
    - `+`
    - `-`
    - `*`
    - `quotient`
    - `remainder`
    - `=`
    - `<`
    - `>`
    - `cons`
    - `car`
    - `cdr`
    - `set-car!`
    - `set-cdr!`
    - `list`
    - `eq?`


* v0.11 Added the `+` primitive procedure
* v0.10 Added the `if` form
* v0.9  Added environment, variables, definitions and assignments
* v0.8  Added quoted expressions
* v0.7  Symbols
* v0.6  Support for lists and pairs
* v0.5  Implemented the empty list `()`
* v0.4  Support for characters and strings
* v0.2  Support for the boolean values `#t` and `#f`
* v0.1  The reader can handle integers