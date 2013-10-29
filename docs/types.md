# Type System

## Basic Types

### Integers

Hylas integers can be of any bit-width between 1 (Boolean) and 2<sup>23</sup>-1, but
[type legalization](http://blog.llvm.org/2011/12/llvm-31-vector-changes.html) will convert integers to the
nearest lossless representation (ie, `i12` will likely become `i32` or `i16`).

* `i1`: Boolean, can only be true and false.
* `i32`: C's `int`.
* `ui64`: C's `unsigned long long`.
* `i128`: A very large integer.

The symbol `word` is aliased to the integer type that represents a machine word. In 64-bit systems, this is
usually `i64`.

### Floating-Point

* `half`: Halfprecision, 16 bits wide.
* `single`: Single precision, 32 bits wide.
* `double`: Double precision, 64 bits wide.
* `quad`: Quadruple precision, 128 bits wide.

## Aggregate Types

### Tuples

### Records

### Unions

## Function Pointers

## Abstract Types

## Generic Types

## Type Expressions

A type expression is a special kind of expression that can only appear inside type signatures. It can be
used to 'transform' types, for example, to extract the *n*th type of a tuple or the return type of a function
pointer type.

* `(pointer type)`: Increases the indirection level of `type` by one. In C terms: `pointer(char) -> char*`.
* `(unpointer type)`: The reverse of the above.
* `(base type)`: If `type` is a pointer of any indirection (eg, pointer to pointer to ...), return the base type.
In C terms: `base(char***) -> char`.
* `(fn t1 t2 ... tn ret)`: Create a function pointer type. `t1` to `tn` are the types of the arguments, and `ret`
is the return type.
* `(tup t1 t2 ... tn)`: Create a tuple of types `t1` to `tn`.
* `(rec (name1 t1) ... (namen tn))`: Create a record type, where each argument is a `name,type` pair.
* `(type form)`: The type of the expression `form`.
* `(ret fn-type)`: Extract the return type from a function pointer type: `(ret (fn i32 i32 i8*)) -> i8*`.
