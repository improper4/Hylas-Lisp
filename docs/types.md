# Type System

## Basic Types

### Integers

`i1`: Boolean, can only be true and false.
`i32`: C's `int`.
`ui64`: C's `unsigned long long`.
`i128`: A very large integer.

Note that after [type legalization](http://blog.llvm.org/2011/12/llvm-31-vector-changes.html), most integer types that can't be represented directly will be 'legalized' into their nearest lossless representation, i.e: `i1` will, with all probability, become `i8`.

The symbol `word` is aliased to the integer type that represents a machine word. In 64-bit systems, this is usually `i64`.

### Floating-Point

`half`: Halfprecision, 16 bits wide.
`single`: Single precision, 32 bits wide.
`double`: Double precision, 64 bits wide.
`quad`: Quadruple precision, 128 bits wide.

## Aggregate Types

### Tuples

### Records

### Unions

## Function Pointers

## Abstract Types

## Generic Types

## Type Expressions

A type expression is a special kind of expression that can only appear inside type signatures. It can be used to 'transform' types, for example, to extract the *n*th type of a tuple or the return type of a function pointer type.
