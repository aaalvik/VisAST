# Parse tree and evaluation visualizer

This is a school project where the goal was to visualize a parse tree and evaluation of my own language step-by-step using Elm. I implemented a simple imperative language, which I then defined both a big-step and a small-step evaluator for. I used the big-step evaluator for getting the final result quickly, and the small-step for visualizing every step during evaluation. 

### Syntax for the language: 
```
stmt := <expr> `;` <stmt> | <expr>

expr 
    := 
     | <int>                                          // integer
     | <var>                                          // variable
     | <expr> * <expr>                                // multiplication
     | <expr> + <expr>                                // addition
     | <expr> - <expr>                                // subtraction
     | <expr> < <expr>                                // less than
     | <expr> > <expr>                                // bigger than
     | <expr> == <expr>                               // equality
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

* Imperative language
* No type checking
* All values are immutable
* Static scoping
* Higher order functions
* Only integer types

There is no type safety here, but the purpose of this project was to visualize parse trees and step-by-step evaluation, so that was not a priority. 
