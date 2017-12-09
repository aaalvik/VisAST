# Abstract syntax tree and evaluation visualizer

This is a school project where the goal was to visualize an abstract syntax tree and evaluation of my own language step-by-step using Elm. I implemented a simple imperative language, which I then defined both a big-step and a small-step evaluator for. I used the big-step evaluator for getting the final result quickly, and the small-step for visualizing every step during evaluation. 

### Syntax for the language: 
```
stmt := <expr> `;` <stmt> | <expr>

expr 
    := <int>                                          // integer
     | <var>                                          // variable
     | <expr> `*` <expr>                              // multiplication
     | <expr> `+` <expr>                              // addition
     | <expr> `-` <expr>                              // subtraction
     | <expr> `<` <expr>                              // less than
     | <expr> `>` <expr>                              // bigger than
     | <expr> `==` <expr>                             // equality
     | `if` <expr> `then` <expr> `else` <expr>        // if-sentence
     | `set` <var> `=` <expr>                         // variable assignment
     | function <var> `(` <argNames> `)` `=` <expr>   // function definition
     | <var> `(` <args> `)`                           // function application

int := [0-9][0-9]*

var := [a-z][a-zA-Z0-9\-_]*

argNames := [<var>]*

args := [<expr>]+
```

### General language features

* **Imperative language**<br/>
  You can create a sequence of expressions, and variables/functions defined in an expression will be in scope for expressions that come later in the sequence. 
* **No type checking**<br/>
  There is no type safety in my language yet, but the purpose of this project was to visualize abstract syntax trees and step-by-step evaluation, so that was not a priority.
* **Immutable data and lexical scoping**<br/>
  All values are immutable, like in haskell. You can set a variable that exists to something else, but this will be a new copy, not altering the variable. When returning from an inner scope the inner environment is discarded. 
* **Higher order functions**<br/>
  Functions can take functions as arguments. Not yet implemented lambdas. 
