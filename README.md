# Thorn

Thorn is a mixed paradigm language that has
imperative-like functions with a huge emphasis
on FP concepts such as: function purity,
immutability, partial application, etc.

## Intro
### Hello, World!
A "Hello, world!" program in Thorn looks like this:

```Thorn
1 | module Entry where
2 |
3 | export impure fn entry => Array String, Void;
4 | entry _ {
5 |     println "Hello, World!";
6 | }
7 |
```

On line 1, we name the module `Entry` to
signal to the compiler that this file contains
the program entry point.

On line 3, we declare the type of the entry point
function. Functions in Thorn are what are called
"Curried". They behave the same way as Haskell
functions in that `add a b` is the same as `(add
a) b`. We can worry about what `export` and
`impure` mean soon.

On line 4, we begin the definition of the entry
function. Its single paramater (which is of type
`Array String`) is ignored by using a single
underscore in place of an identifier

Finally, on line 5, we print the string "Hello,
World" to the standard output (usually the
console)


### Important Concepts
- #### Immutability
    - All variables are immutable by default.
    - arguments passed to pure functions are
      passed by immutable reference.
- #### Currying / Partial Application
    - `f x y z` is effectively the same as `(f x)
      y z)` and `(f x y) z`.
- #### Function Purity
    - A function is considered pure if:
        - it results in no side effects, and
        - given the same inputs, returns the same
          output.
    - A function is considered impure if:
        - it results in side effects, or
        - it is able to return a different result
          given the same input.
    - Pure functions can only call other pure
      functions.
    - Impure functions can call any function.
    - Unsafe functions can call any function, and
      disguise themselves as pure to the compiler
- #### Datatypes
    - A data-type holds structured information.
    - A data constructor can be thought of as one
      possible form that the data can take.
- #### Laziness??
- #### Traits??

## Primitive Types
- `Boolean`
- `Int`/`UInt`
    - smallest size (signed/unsigned respectively)
    integral type needed at the given time.
    - Width can change at runtime??
- `Char`
- `IntN`/`UIntN` (where 'N' is a power of 2)??
    - (signed/unsigned respectively) integeral
    type of bit-width 'N'
- `Float`/`Double`
    - Floating (decimal) point values of width
    32 and 64 respectively
- `String`
    - Array length is stored immediately before
    the array in memory for O(1) length lookup
- `Array a`
    - 0-based index
    - Fixed size

## Examples
### Binary Search Tree
Thorn is great for recursive data structures such
as Binary Search Trees. Let's define a data type
for such application.

```Thorn
module Data.Tree where

export data Tree t
    := export Empty
    |= export Node => t, Tree t, Tree t
    ;
```

We begin by declaring the module name for the
linker. We then declare a public data type 
(hence the `export data`) called `Tree` that
takes one type-parameter `t`. Next, we begin to
define constructors for `Tree`. Data constuctors
can be thought of as the set of all possible forms
that the data can take. In this case, a Tree can
either be empty because it contains no data nor
subtrees, or it can be a Node which contains a
value of type `t`, and two subtrees of type `Tree
t`.

Notice the syntactical similarities between the
a function type declaration, and a constructor
declaration: this is because constructors can behave
very similarily to functions. It would not be
wise to curry a constructor (i.e. `(Node 7 Empty)
some_tree`) because if a peice of the constructor is
missing, it will cause a runtime error. It is
possible, however.

Let's now write some functions to manipulate trees.

```Thorn
extern pure fn new => a, Tree a;
new a { return (Node a Empty Empty); }

extern pure fn is_empty => Tree a, Boolean;
is_empty (Empty) { return True; }
is_empty _ { return False; }
```

### Linked List

```Thorn
module Data.List where

export data List a
    := export Empty
    |= export Node => a, List a
    ;

export pure fn new => a, List a;
new x { return (Node x Empty) }

export pure fn insert => a, List a, List a;
insert x (Empty) { return (new x); }
insert x (Node v n) { return (Node v (insert x n)); }
```
