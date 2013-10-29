![Logo](http://eudoxia0.github.com/Hylas-Lisp/img/logo.svg)

Hylas is a statically-typed, [wide-spectrum](http://en.wikipedia.org/wiki/Wide-spectrum_language), JIT-compiled dialect of Lisp that combines the performance and control of low-level languages with the advanced metaprogramming features of languages like Lisp.

# Examples

## Recursive Fibonacci Function:

```lisp
(function fib ((n i64)) i64
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))
```

## Calling a foreign function:

```lisp
(foreign C printf (pointer i8) ... i32)

(printf "Hello, world! This is a double, in scientific notation: %e" 3.141592)
```

## Using a foreign library:

```lisp
(link "libSDL.so")

(foreign C SDL_Init i64 void)
(foreign C SDL_Delay i64 void)
(foreign C SDL_Quit void)

(type SDL_Color
  (structure
    (r            byte)
    (g            byte)
    (b            byte)
    (unused       byte)))
```

# Documentation

Documentation on the _language_ is available as a series of [Markdown](http://daringfireball.net/projects/markdown/) files in the `docs` folder, and can be built using Make and [Pandoc](http://johnmacfarlane.net/pandoc/):

```bash
$ make book
```

This will generate the HTML files in the `docs/book/html` folder. Use `make clean` to delete them.

Documentation on the _compiler_ can be generated running `make doc`.

# License

Copyright (C) 2012 Fernando Borretti

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
