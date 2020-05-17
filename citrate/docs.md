

There is no AST, just a list of tokens. Tokens are one of the following:

  name          | re   | desc
  ----          | --   |  ----
OpenLambda      | '{'  | this is the start of a lambda block
CloseLambda     | '}'  | this is the end of a lambda block
IntToken Int    | \d+  | an integer literal.
OperatorToken   | [-+] | an operator, preforms a specified action when executed
LambdaToken     |      | a parsed version of a lambda
IdentifierToken | \w+  | a keyword or register


the lexer will return a list of tokens (but no lambda tokens).
The parser does basic analysis and converts the (Open|Close)Lambda Tokens into LambaTokens.

the program is a list of "tokens".
Each token can be executed, and when it is, it manipulates the state of the program.
Tokens are executed sequencially.

## The state

The state of the program is pretty simple.
There is a stack of values, each value being either an integer or a lambda.
There is 26 registers, labeled a through z, each with a value (again either an integer or a lambda)  

Each instruction just manipulates this state - there is no return or input values

### Integer literals

Integer literals, for example `1` or `7`, are pushed to the stack when executed.

> NOTE: only numbers 0-9 supported rn

### Oerations

Operators, like - or +, pop the top two elements off the stack and then push the result back on


### Built-in functions

`in` reads in a number from stdin (puts it on the stack), `out` pops a value and prints it to stdout.

`run` pops the lambda on the top of the stack and runs it

### Lambdas

Lambdas are a pair of brackets containing a list of tokens. When the lambda is passed over in code, it is pushed to the stack, and can be ran with `run`.

### Registers

`[a-z]` takes the value in the register specified and pushes it to the stack.
`[A-Z]` pops a value of the stack and puts it in the register specified, discarding the old value.

### Example programs

*this will be in the format, ($STDIN -> )? $CODE => $STDOUT ($NOTES)?*

`1 out` => `1`
`7 out` => `7`
`1 1 + out` => `2`
`7 4 - out` => `3`      *this is because it's 7 minus 3, not 3 minus 7*
`7` -> `in out` => `7`    *very basic cat program*

`1 A a out` => `1`      *this pushes 1, pops it into the a register, then pushes it back to the stack*

`7 A a a out out` => `7 7` *a basic duplicator: `A a a`*

`{1} run out` => `1` *pushes the lambda to the stack, pops it and runs it. the content of the lambda when executed push `1` to the stack, which is then printed*

`{{{7 out}}} run run run` => `7` *this pushes a lambda to the stack, runs it, which does the same, and again, till the third lambda prints 7*


### If statements

Until now, we have had no branching - ie, no control flow

this makes things rather difficult, as programs will execute the same things every time ran.

`if` is a keyword that allows for branching control flow.

when `if` is executed, it'll pop two values off the stack.
If the A register is 0, it'll push the first back on, otherwise it'll push back the second

For example:

`7 A 3 4 if out` => `4`
`0 A 3 4 if out` => `3`
`0` -> `in A 3 4 if out` => `4`
`1` -> `in A 3 4 if out` => `3`


### files

so in the past, we've only have an input of a line of tokens.

Citrate also supports the file format, `*.ct`. In each file format is the several lines.
Each line follows this syntax

DEF $main = $tokens~

DEF main = 7 out









