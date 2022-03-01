# Rose

Rose is a mixed paradigm language that has
imperative-like functions with a huge emphasis
on FP concepts such as: function purity,
immutability, partial application, etc.

## Intro
### Hello, World!
A "Hello, world!" program in Rose looks like this:

```Rose
1 | module Entry where
2 |
3 | export impure fn entry => Array String, Exit;
4 | entry args {
5 |     println "Hello, World!";
6 | }
7 |
```

On line 1, we name the module `Entry` to
signal to the compiler that this file contains
the program entry point.

On line 3, we declare the type of the entry point
function. Functions in Rose are what are called
"Curried". They behave the same way as Haskell
functions in that `add a b` is the same as `(add
a) b`. We can worry about what `export` and
`impure` mean soon.

On line 4, we begin the definition of the entry
function. Its single paramater (which is of type
`Array String`) will not be used, and thus is
ignored

Finally, on line 5, we print the string "Hello,
World" to the standard output (usually the
console). We do not have to explicitly exit
because an exit code of 0 (indicating success)
is used by default when the code reaches the end
of the entry function.


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
    integral type needed at the given time
    - Width can change at runtime??
- `Char`
- `IntN`/`UIntN` (where 'N' is a power of 2)??
    - (signed/unsigned respectively) integeral
    type of bit-width 'N'
- `Float`/`Double`
    - Floating (decimal) point values of width
    32 and 64 respectively
- `String`
    - Exactly the same as Array a
- `Array a`
    - Contiguous memory layout
    - Fixed size
    - 0-based index
    - Array length is stored immediately before
    the array in memory for O(1) length lookup

## Examples
### Binary Search Tree
Rose is great for recursive data structures such
as Binary Search Trees. Let's define a data type
for such application.

```Rose
module Data.Tree where

export data Tree a
    := Empty
    |= Node => a, Tree a, Tree a
    ;
```

We begin by declaring the module name for the
linker. We then declare a public data type 
(hence `export data`) called `Tree` that takes one
type-parameter `t`. Next, we begin to define
constructors for `Tree`. Data constuctors can be
thought of as the set of all possible forms that
the data can take. In this case, a Tree can either
be empty because it contains no data nor subtrees,
or it can be a Node which contains a value of type
`t`, and two subtrees of type `Tree t`.

Notice the syntactical similarities between the
a function type declaration, and a constructor
declaration: this is because constructors can behave
very similarily to functions. It would not be
wise to curry a constructor (i.e. `(Node 7 Empty)
some_tree`) because if the program tries to access
a peice of the constructor that is missing, it will
cause a runtime error. It is however possible.

Let's now write a function to insert values:
```Rose
export pure insert =>  { Ord a }, a, Tree a, Tree a;
insert a [Node v l r] {
    match (a <=> v) {
        [LT] { return (Node v (insert a l) r); }
        [GT] { return (Node v l (insert a r)); }
        [EQ] { return (Node v l r); }
    };
}
```
