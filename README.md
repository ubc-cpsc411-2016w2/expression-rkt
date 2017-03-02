# expression-rkt
Interpreter for the Expression Language in Racket

The interpreter itself (interp.rkt/ast.rkt) is a mostly-unexciting
311-style interpreter.  The interesting part is everything leading up
to it:
1) Scanning/Lexing (scanner.rkt/token.rkt)
2) Parsing (parser.rkt/parse-tree.rkt)
3) Tree Abstraction/Parsing Actions (tree-abstraction.rkt)

The scanner realizes the principles of Chapter 2 of the textbook,
implementing a state machine as a collection of mutually
tail-recursive functions that use accumulators to manage input
characters and token-matching

Parsing makes explicit the idea of a parse-tree/concrete syntax tree.
Most production compilers do not bother explicitly creating a parse
tree, but the concept is an important distinction: your LL(1)  parser
grammar describes three things at the same time:

1) a family of token strings;
2) a data definition for parse trees, each of which represent a token string;
3) a procedure for translating representable token strings into parse
trees.

Tree Abstraction makes explicit what parsing actions do: implement
*some* structural recursion over parse trees.  In this particular
case, we convert a parse tree into an abstract syntax tree, which is
what we wanted all along.

JavaCC specifies all three of Scanning, Parsing, and Tree Abstraction
in one file with two sections.


Running the code:
If you have racket on your path, you can use the 'run' script as:
  run file.exp


