(in-package :hylas)

#|

In short, the usage is:

´´´lisp
(extract form (s_1 s_2 s_3 ... s_n)
    ...code...)
´´´

This will bind s_n to the register that holds the result of the n-th element in ´form´,
and s_n-type to the type of said form.

To emit the code for some arbitrarily long form, and store the registers and their types as a p-list,
simply:

´´´lisp
(extract-list form
    ...code...)
´´´
|#

(defun extract-backend (forms bindings code)
    "Form is a list of forms to be evaluated, bindings is a list of symbols that will be bound to the result of evaluating the forms in order."
    `(let* ((code (emit-code ,(car forms)))
        (,(car bindings) (res-version code))
        (,(read-from-string (concatenate 'string (symbol-name (car bindings)) "-type")) (res-type)))
    ,@(if (cdr forms)
        (list (extract-backend (cdr forms) (cdr bindings) code))
        code)))

(defmacro extract (forms (&rest bindings) &rest code)
    `(extract-backend ,forms ',bindings ',code))

;; Variables

(defbuiltin def
    (let ((sym (symbol-name (nth 1 form))))
        (extract (cddr form) (value)
            (if (lookup sym)
                (raise form "Symbol '~a' already defined in the present scope." sym)
                (progn
                    (var sym (var res-type))
                    (append-entry
                        (assign (emit "%~a" sym) (res))))))))

(defbuiltin global)

(defbuiltin set)

;; Flow Control

(defbuiltin if
    (extract (test true-branch false-branch)
        (if (not (boolean? test-type))
            (error "The type of the test expression to (if) must be i1 (boolean).")
            (if (match-type true-branch-type false-branch-type)
                ;match

                ;no match
                (error "The types of the true and false branches must match")))))

(defbuiltin begin
    (with-new-scope
        (extract-list)))

;; Mathematics

(defmacro generic-twoarg-op (op)
    `(extract form (first second)
        (if (match-type first-type second-type)
            (assign (res first-type) (emit "~a ~a ~a, ~a ~a" ,op first-type first second-type second))
            (error "Types must match")))

    (defmacro make-math-operations ()
        `(progn
            ,@(loop for operator in '(add fadd sub fsub mul fmul udiv sdiv fdiv urem srem)
                collecting `(defbuiltin ,operator
                    (generic-twoarg-op ,(symbol-name operator))))))
    (make-math-operations)


;; Bitwise Operations

(defbuiltin shl
    (generic-twoarg-op "shl"))
(defbuiltin lshr
    (generic-twoarg-op "lshr"))
(defbuiltin ashr
    (generic-twoarg-op "ashr"))
(defbuiltin bit-and
    (generic-twoarg-op "and"))
(defbuiltin bit-or
    (generic-twoarg-op "or"))
(defbuiltin bit-xor
    (generic-twoarg-op "xor"))

(defbuiltin byte-swap)
(defbuiltin count-leading-ones)
(defbuiltin count-trailing-ones)
(defbuiltin truncate)
(defbuiltin extend)
(defbuiltin sextend)
(defbuiltin zextend)

;; Conversion

(defbuiltin ptr->int)
(defbuiltin int->ptr)
(defbuiltin bitcast)
(defbuiltin coerce)

;; Bitfield size

(defbuiltin size)
(defbuiltin actual-size)

;; Data structures

(defbuiltin structure)

(defbuiltin struct-nth)
(defbuiltin struct-access)

(defbuiltin make-array)
(defbuiltin global-array)
(defbuiltin nth-array)

;; Memory

(defbuiltin mem-allocate)
(defbuiltin mem-store)
(defbuiltin mem-load)

(defbuiltin create)
(defbuiltin reallocate)
(defbuiltin destroy)

(defbuiltin address)

;; FFI

(defbuiltin link)
(defbuiltin foreign)

;; LLVM and Assembler

(defbuiltin asm)
(defbuiltin inline-asm)
(defbuiltin LLVM)
(defbuiltin inline-LLVM)
