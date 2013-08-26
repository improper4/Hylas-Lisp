# Reference

## Special Forms

Special forms, also known as operators, are non-overloadable operators which
usually have special execution rules. The simplest example is `if`, which only
executes one of either the true and false branches, depending on the result of a
condition, and `def`, which does not try to evaluate the symbol it's going to
define.

### `def` and `let`

```lisp
(def|let {var_1 value_1 [docstring]}+)

```

Examples:

```lisp
(def
  x 0
  y 3.14 "A shitty approximation of pi"
  z {65 65 65} "A triple of integers")
```

### `set`

### Flow Control

#### `if`

```lisp
(if test true-branch false-branch)
```

Both branches must be of the same type.

#### `tagbody` and `go`

`tagbody` is similar to its Common Lisp counterpart, only far more limited to reduce Dijkstra's disappointment at me. It's main purpose is to allow low-level performance hacks and allow higher-level iteration constructs to be implemented in Hylas itself.

`go` is considered to 'return' true.

```lisp
(tagbody
  {(tag_1 {code_1}+)}+)
```

`go` cannot cross scopes. The following will not compile:

```lisp
(tagbody
  (start
    (tagbody
      (start
        (go next))))
  (next true))
```

Example:
```tagbody
(tagbody
  (start
    (if (some-computation)
        (go success)
        (go error)))
  (success true)
  (error
    (out "An error has occurred")
    false))
```

### Functions

#### `function`

#### `lambda`

#### `foreign`

#### `fn`

### Types

#### `type`

```lisp
(type type-name type-definition [docstring])
```

Defines a new type `type-name`.

#### `size`

#### `ptr->int` and `int->ptr`

#### Integer Constructors

#### Tuple Constructors

#### Array Constructors


### Memory

#### `create`

#### `reallocate`

#### `destroy`

#### `defmemman`

#### `address`

### Inlining

These operations allow the programmer to directly inject LLVM IR or platform-dependent
assembly into the code. `llvm` and `inline-llvm` insert LLVM Ir globally and locally, respectively; `asm` and `inline-asm` insert assembly code, again, globally and locally.

## Language Core

The language core consists of operators that are called after a user-defined function with the name and prototype could not be found. They have no special execution rules and can be overloaded.

### Mathematical Operations

All mathematical operations have an arity of two unless otherwise specified, and
don't support mixing integer and floating-point operands. These functions check
whether their operands are signed or ordered and whenever possible optimize the operations with this information. All arguments must be either integers, floats,
vectors of integers or vectors of floats.

`+`, `-`, `*`, `/`, `%` are addition, substraction, multiplication, division
and modulo, respectively.

`=`, `<`, `<=`, `>`, `>=` should be self-explanatory.

### Bitwise Operations

Both arguments must be integers or vectors of integers.

`shl`: Shift the first argument *n* bits to the left, where n is the second
argument.
`shr`: Shift the first argument *n* bits to the right, where *n* is the second
argument. Whether the first argument is a signed or unsigned integer determines
whether this is logical or arithmetic right-shift, respectively.
`bit-and`: Bitwise AND.
`bit-or`: Bitwise OR.
`bit-xor`. Bitwise XOR.

`count-ones`: Return the [Hamming weight](http://en.wikipedia.org/wiki/Hamming_weight) of the first argument.
`count-leading-ones`: Count the number of most significant zeros.
`count-trailing-ones`: Count the number of least significant zeros.

### Linking

#### `link`

```lisp
(link library)
```

Dynamically links to `library`, which must be a string literal.

Example:

```lisp
(link #+windows "libSDL.dll" #+unix "libSDL.so" #+darwin "libSDL.dylib")
```


## Prelude

The prelude consists of Hylas code that is compiled prior ot everything else.

### `show`

## Type Expressions

## Compiler Options
