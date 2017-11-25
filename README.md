# Roller Script REPL

This is a reference implementation of a REPL for Roller Script language.

Roller Script is a toy language project of mine, and it's name will probably change in the future.
It is not designed to be a particularly useful language, at the moment, but even this might change.

There is no language reference yet, but you might want to check out the syntax description file at src/parser/expr.lalrpop for a better understanding of the current syntax.
It is written in the [LALRPOP](https://github.com/nikomatsakis/lalrpop) rule syntax.

## What is implemented
* Comments
    * C-style, with nesting block comments (like in Rust)
    * `//` ignores text till end-of-line and all text enclosed between `/*` and _the matching_ `*/` is ignored
    * Maximum block comment nesting depth is 255, which is plenty
* Datatypes
    * None (`none`)
    * Boolean (`true`, `false`)
    * Numerals
        * Only 32-bit ratios atm
        * Accepts exponent syntax, like `2.3e4` or `1e-2`
    * Strings
        * No string operations yet
        * String escaping works, but even escaped backslashes are not allowed before the ending quotation mark
            * (like in Python)
    * List (`[1, 2, 3]`)
        * Implemented as a Rust vector
    * Set (`{1, 2, 3}`)
        * Implemented as a Rust binary tree set
    * Map (`{1:"a", 2:"b", 3:"c"}`)
        * Implemented as a Rust binary tree map
    * Distribution (`{1 | 2 | 3}` or `{true:2 | false:1}`)
        * A weighted discrete distribution, implemented as a binary tree map, like the previous map datatype
        * Each element allows an optional weight value, that has to be an integer
        * Duplicate key values are merged and the result's weight value will be their sum
    * Functions (`{a b -> a + b}`)
        * Functions are first-class citizens, which is a trendy way of saying that they are just another datatype in this language
* Variables
    * Variable identifiers are unicode strings
    * Identifiers start with any character in the word unicode class or underscore, followed by any number of characters from word and number unicode classes and underscores
    * Matched with `[\pL_][\pL\pN_]*` regex
* Arithmetic operations
    * Addition (`+`), substraction, negation (`-`), multiplication (`*`), division (`/`), exponentiation (`^`)
    * Only defined between numerals
* Boolean operations
    * `not`, `and`, `or`, `xor`
    * Only defined for booleans, for now
* Comparison
    * Equality and inequality: `a is b`, `a isnt b`
    * Allowed comparison operators are `is`, `isnt`, `<`, `>`, `<=` and `>=`
    * Returns boolean
* Function call
    * `foo(1, 2)`, `{x y -> x + y*2}(1, 2)`
    * Also used for collection indexing
* If-expression
    * `if x is 2 then "hello" else "hi"`
    * The conditional is any expression that evaluates to a boolean
    * Else-part is required (for now)

## What is not yet implemented
* So many things
* File input
* Error catching and throwing
* Command-line arguments and REPL-variables
    * Will use Clap
    * Currently always prints debug representations of the tokens and the AST unless those prints are commented
* Control structures and a solution for sequential code execution (code blocks? semicolon operator? just use lists?)
    * Loops
* Builtin functions
    * `print`, `println`, `read` and `readln`
        * Could input reading operations exploit the lexer tokens?
    * String operations
    * Collection operations
* Piecewise and assignment operations
    * Easy to implement
* Distribution operations
    * Print distributions nicely, show probabilities, and resolve random outputs
* Integer logical operations, including bitshifts and maybe rotations
    * Would work best with the type system (these are not defined for ratios)
* Type system, type constraints
    * Pretty big one
    * Probably after finishing the "MVP"
* List comprehensions
    * Needs type system probably
* Other numerals beside ratios
    * To stop crashing when you type large numbers like `1e10`
    * Bigratios
    * Normal integers and bigints
    * Floats
        * Meeting a NaN value should throw an error
    * Might need the type system for most idiomatic implementation
* Maybe use reference counted or copy-on-write values, or maybe even garbage collecting
    * Current implementation is pretty inefficient and copies data a lot
* References
    * Maybe even move from copy-semantics to reference-semantics
* Using symbol operators as prefixed function names
    * Prefixed with the backtick character ( ` )
* Functions as infix operators
    * Also prefixed with the backtick
* Some unit tests...

## Possible plans for future
* Separate project into multiple crates
    * Parser
    * AST
    * Library
    * Interpreter and REPL
* Dynamic library loading
* Module/library system
    * Move as many builtins as possible into a prelude module
* Physical unit system/library
    * Bits and bytes, with SI and mebi- kibi- etc prefixes that might be built-in
* LLVM IR compiler frontend
