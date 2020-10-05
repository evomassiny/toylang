# Toylang
This a toy programing language, **utterly useless**.

I'm using it to experiment parser, compiler and interpreter designs (as I don't know much about it).

To test the interpreter you can run: 
```
cargo run ./source.js
```

The supported syntaxe is a small subset of the javascript one:
* keywords:
    * `function` (only supports named function)
    * `while`
    * `if`
    * `else`
    * `return`
    * `let`
    * `null`
    * `undefined`
    * `continue`
    * `break`
* operators:
    * `+ - * / ** %`
    * `> < >= <= ==`
    * `! && ||`
    * `++foo foo++ --foo foo--` 
* separators:
    * `;` for expressions
    * `( )` for sub-expressions
    * `{ }`
* litterals:
    * booleans
    * float/integers
    * strings

(see ./source.js for an example)

# Credits
 * This repo takes huge inspirations from the [**boa**](https://github.com/jasonwilliams/boa) interpreter.
 * The lexer was inpired by the [**regex**](https://github.com/rust-lang/regex/blob/master/HACKING.md) crate documentation.
