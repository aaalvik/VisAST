# Simple language

### Syntax: 

expr 
    := <expr> * <expr> 
     | <expr> + <expr>
     | <expr> - <expr> 
     | <expr> < <expr> 
     | - <expr> 
     | if <expr> then <expr> else <expr> 
     | set <var> = <expr> 
     | function <var> <args> = <expr>
     | <var> ( <argVals> )


var := [a-z][a-zA-Z0-9_]*
args := [<var>]*
argVals := [<expr>]+
