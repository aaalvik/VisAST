# Abstract syntax tree and evaluation visualizer

This is a school project where the goal was to visualize an abstract syntax tree and evaluation of my own language step-by-step using Elm. I implemented a simple imperative language, which I then defined both a big-step and a small-step evaluator for. I used the big-step evaluator for getting the final result quickly, and the small-step for visualizing every step during evaluation. 

### Installation and Use

* You need the [npm](https://www.npmjs.com/) JavaScript package manager.
* You also need [Elm](http://elm-lang.org/) – `npm install elm`.
* You also need a whole bunch of dependencies – these can be installed by running just `npm install` inside the ASTVisualiser directory.
* Once all dependencies are installed, you can start the application with `npm start`.
* Point your browser to [localhost://8080/](localhost://8080) and enjoy!

Here are a few expressions you can try (see the syntax description below for more possibilities):
```
3*4+2+5+6*9
set x = 3*4; x*9
set x = 3*4; set y = x+12; x+y*10
function f(x) = x*x; f(2)
```

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
* **Immutable data**<br/>
  All values are immutable, like in haskell. 
* **Lexical, nested scoping**<br/>
  Variables can be shadowed. When returning from an inner scope the inner environment is discarded. 
* **Higher order functions**<br/>
  Functions can take functions as arguments. Not yet implemented lambdas. 

### Bugs

* Recursion doesn't work at the moment
