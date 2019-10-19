# Toylang
This a toy programing language, **utterly useless**.

I'm using it to experiment parser designs.

It's only a parser, It cannot execute anything!

To test the parser you can run: 
```
cargo run -- ./source.toy
```

The supported syntaxe is a small subset of the javascript one:
* keywords:
    * `function` (only support named function)
    * `while`
    * `if`
    * `else`
    * `return`
    * `let`
* operators:
    * `+ - * / ** %`
    * `> < >= <= ==`
    * `! && ||`
* separators:
    * `;` for expressions
    * `( )` for sub-expressions
    * `{ }`

(see ./source.toy for an example)
