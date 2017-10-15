# Roller Script (name subject to change) REPL

This is a reference implementation of a REPL for Roller Script language.

Roller Script is a toy language project of mine, and it's name might change in the future.
It is not designed to be a particularly useful language, at the moment, but even this might change.

## What is implemented
* Datatypes
    * None (`none`)
    * Boolean (`true`, `false`)
    * Numerals
        * 32-bit ratios atm
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
        * Functions are first-class citizens
* Variables
    * Variable identifiers are unicode strings
    * Identifiers start with any character in the word unicode class or underscore, followed by any number of characters from word and number unicode classes and underscores.
    * Matched with `[\pL_][\pL\pN_]*` regex
* Arithmetic operations
    * Addition (`+`), substraction, negation (`-`), multiplication (`*`), division (`/`), exponentiation (`^`)
    * Only defined between numerals
* Boolean operations
    * `not`, `and`, `or`, `xor`
    * Only defined for booleans, for now
* Comparison
    * `is a = b`
    * Allowed comparison operators are `=`, `<`, `>`, `<=`, `>=` and `!=` (inequality)
    * Returns boolean
* Function call
    * `foo(1,2)`, `{x y -> x + y*2}(1, 2)`

## What is not yet implemented
* So many things
* Comments
    * C-style probably
* Command-line arguments and REPL-variables
    * Will use Clap
* Collection indexing and other functions
* Control structures and a solution for sequential code execution (code blocks? semicolon operator? just use lists?)
* String operations
* Piecewise and assignment operations
    * Easy to implement
* Distribution operations
    * Print distributions nicely, show probabilities, and resolve random outputs
* Integer logical operations, including bitshifts and maybe rotations
    * Would work best with the type system (these are not defined for ratios)
* Type system, type declarations
    * Pretty big one
    * Probably after finishing the "MVP"
* Other numerals beside ratios
    * To stop crashing when you type large numbers like `1e10`
    * Normal integers and bigints
    * Floats
    * Bigratios
    * Might need the type system for most idiomatic implementation
