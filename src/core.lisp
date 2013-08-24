(in-package :hylas)
(annot:enable-annot-syntax)

@document "Defines special forms and code language functions."

(defmethod construct-integer (type form (code <code>))
  (if (integerp (car form))
      ;; Integer literal, so we just write it straight into the code
      (append-entry code
        (assign (res code type)
          (constant type (car form))))
      (extract form (val)
        (append-entry code
          (assign (res code type)
            (cond
              ((integer? val-type)
                ;; All we have to do is check the bit-width and cast things
                ;; appropriately
                (cond
                  ((< (width val-type) (width type))
                    (if (signed? type)
                        ;; Signed extension
                        (conv "sext" val val-type type)
                        ;; Unsigned extension
                        (conv "zext" val val-type type)))
                  ((> (width val-type) (width type))
                    ;; Truncation
                    (conv "trunc" val val-type type))
                  (t
                    ;; No operation needed, cause trichotomy
                    (constant type val))))
              ((float? val-type)
                ;; Convert to an integer using fpto[su]i
                (if (signed? type)
                    (conv "fptosi" val val-type type)
                    (conv "fptoui" val val-type type)))
              (t
                (raise form "Can't convert '~A' to an integer." val-type))))))))

(defmethod construct-float (type form (code <code>))
  (if (floatp (car form))
      (append-entry code
        (assign (res code type)
          (constant type (car form))))
      (extract form (val)
        (append-entry code
          (assign (res code type)
            (cond
              ((float? val-type)
                (cond
                  ((< (width val-type) (width type))
                    (conv "fpext" val val-type type))
                  ((> (width val-type) (width type))
                    (conv "fptrunc" val val-type type))
                  (t
                    (constant type val))))
              ((integer? val-type)
                (if (signed? val-type)
                    (conv "sitofp" val val-type type)
                    (conv "uitofp" val val-type type)))
              (t
                (raise form "Can't convert '~A' to a float." val-type))))))))

@doc "Please don't look at this code. Just don't. Please forgive me."
(defmacro extract (form (&rest bindings) &rest code)
  (let* ((str (make-string-output-stream))
         (bindings (loop for i from 0 to (1- (length bindings)) collecting
                     `((code (emit-code (nth ,i ,form) code))
                       (,(nth i bindings) (res code))
                       (,(read-from-string
                           (concatenate 'string
                             (symbol-name (nth i bindings)) "-type"))
                         (res-type code)))))
         (len (length bindings)))
    (format str "~{(let* ~A~}~{~S~}" bindings code)
    (dotimes (i len) (write-string ")" str))
    (read-from-string (get-output-stream-string str))))

(defmacro extract-list (form &rest code)
  `(let ((extracted-registers (list))
         (extracted-types (list)))
     (loop for cell in ,form do
       (let ((new-code (emit-code cell code)))
         (push (res new-code) extracted-registers)
         (push (res-type new-code) extracted-types)
         (setf code (copy-code new-code))))
     (setf extracted-registers (reverse extracted-registers))
     (setf extracted-types (reverse extracted-types))
     ,@code))

(defmacro with-new-scope (code-state &rest code)
  `(let ((code (copy-code ,code-state)))
     (push (make-instance '<scope>) (stack code))
     ,@code))

(defparameter *operators* (make-hash-table :test #'equal))
(defparameter *core* (make-hash-table :test #'equal))

(defmacro defbuiltin (table name &rest code)
  `(setf (gethash ,(format nil "~(~A~)" (symbol-name name)) ,table)
    #'(lambda (form code) ,@code)))

(defmacro defop (name &rest code)
  `(defbuiltin *operators* ,name ,@code))
(defmacro defcore (name &rest code)
  `(defbuiltin *core*,name ,@code))

;;; Variables

(defop def
  (let ((sym (symbol-name (nth 0 form))))
    (extract (cdr form) (value)
      (multiple-value-bind (var pos) (lookup sym code)
        (if (eql pos (length (stack code)))
          (raise form "Symbol '~A' already defined in the present scope." sym)
          (progn
            (var sym code (var code value-type))
            (append-entry code
              (assign (emit "%~A" sym) (res code value-type)))))))))

(defop defglobal
  (let ((sym (symbol-name (nth 0 form))))
    (extract (cdr form) (value)
      (multiple-value-bind (var pos) (lookup sym code)
        (if (eql pos 0)
          (raise form "Symbol '~A' already defined in the global scope." sym)
          (progn
            (append-entry
              (append-toplevel code
                (emit "@~A = global ~A zeroinitializer" sym value-type))
              nil)))))))

(defop set
  (let ((sym (symbol-name (nth 0 form))))
    (extract (cdr form) (new-value)
      (let ((var (lookup sym code)))
        (if var
            (append-entry code
              (store new-value-type var new-value))
            (raise form "No symbol '~A' defined." sym))))))

;;; Flow Control

(defop if
  (extract form (test)
    (unless (boolean? test-type)
      (raise form "The type of the test expression to (if) must be i1 (boolean)."))
    (let* ((true-label (new-label code))
           (false-label (new-label code))
           (end-label (new-label code))
           (true-branch-code
             (emit-code (cadr form)
               (append-entry code
                 (list (branch test true-label false-label)
                       (format-label true-label)))))
           (true-reg (res true-branch-code))
           (true-branch-type (res-type true-branch-code))
           (false-branch-code
             (emit-code (caddr form)
                (append-entry true-branch-code
                  (list (goto end-label)
                        (format-label false-label))))))
      (append-entry false-branch-code
        (let ((false-reg (res false-branch-code)))
          (list (goto end-label)
                (format-label end-label)
                  (assign (res code true-branch-type)
                    (phi true-branch-type
                         true-reg true-label
                         false-reg false-label))))))))

(defop not)

(defop or)

(defop and)

(defop xor)

(defop do
  (with-new-scope code
    (extract-list form
      code)))

(defop tagbody)

(defop go)

(defop do)

(defop dotimes)

(defop doarray)

;;; Mathematics

(defmacro generic-twoarg-op (op)
  `(extract form (first second)
    (if (match first-type second-type)
      (append-entry code
        (assign (res code first-type)
          (cmp ,op "" first-type first second)))
      (error "Types must match"))))

(defop add
  (generic-twoarg-op "add"))
(defop fadd
  (generic-twoarg-op "fadd"))
(defop sub
  (generic-twoarg-op "sub"))
(defop fsub
  (generic-twoarg-op "fsub"))
(defop mul
  (generic-twoarg-op "mul"))
(defop fmul
  (generic-twoarg-op "fmul"))
(defop udiv
  (generic-twoarg-op "udiv"))
(defop sdiv
  (generic-twoarg-op "sdiv"))
(defop fdiv
  (generic-twoarg-op "fdiv"))
(defop urem
  (generic-twoarg-op "urem"))
(defop srem
  (generic-twoarg-op "srem"))

;;; Comparison

(defmacro generic-cmp-op (op valid-tests)
  `(let ((test (car form)))
    (unless (member (symbol-name test) ,valid-tests :test #'equal)
      (error "Unknown test"))
    (extract (cdr form) (first second)
      (if (match first-type second-type)
          (append-entry code
            (assign (res code (int 1))
              (cmp ,op test first-type first second)))
          (error "Types must match")))))

(defparameter +valid-icmp-tests+ (list "eq" "ne" "ugt" "uge" "ult" "ule" "sgt"
                                   "sge" "slt" "sle"))

(defparameter +valid-fcmp-tests+ (list "oeq" "ogt" "oge" "olt" "ole" "one" "ord"
                                   "ueq" "ugt" "uge" "ult" "ule" "une" "uno"))
(defop icmp
  "Integer comparison."
  (generic-cmp-op "icmp" +valid-icmp-tests+))

(defop fcmp
  "Floating-point comparison"
  (generic-cmp-op "fcmp" +valid-fcmp-tests+))

(defop cmp
  "Compare a pair of integers or floats. -1, 0 and 1 for less-than, equal and
  greater-than, respectively. Checks for sign/order by default.")

;; Simpler comparison. Safe by default (Sign and order are checked).

(defop =)
(defop <)
(defop <=)
(defop >)
(defop >=)

;; Unsafe comparisons can be performed by switching off the 'safecmp' option.

;;; Bitwise Operations

(defop shl
  (generic-twoarg-op "shl"))
(defop lshr
  (generic-twoarg-op "lshr"))
(defop ashr
  (generic-twoarg-op "ashr"))
(defop bit-and
  (generic-twoarg-op "bit-and"))
(defop bit-or
  (generic-twoarg-op "bit-or"))
(defop bit-xor
  (generic-twoarg-op "bit-xor"))

(defop count-ones
  "Count the number of set bits in an integer.

  (count-ones b101010) => 3"
  (extract form (source)
    (append-toplevel
      (append-entry code
        (assign (res code source-type)
                (bitop "ctpop" source-type source)))
      (bitop-def "ctpop" source-type))))

(defop count-leading-ones
  (extract form (source)
    (append-toplevel
      (append-entry code
        (assign (res code source-type)
                (bitop "ctlz" source-type source)))
      (bitop-def "ctlz" source-type t))))

(defop count-trailing-ones
  (extract form (source)
    (append-toplevel
      (append-entry code
        (assign (res code source-type)
                (bitop "cttz" source-type source)))
      (bitop-def "cttz" source-type t))))

;; Bitfield size

(defop size
  "Return the size of an object in bytes.

  (size 10) => 8 ;; i64
  (size 3.14) => 8 ;; double
  (size (i8 78)) => 1")

;;; Conversion

(defmacro generic-conversion (op &rest validation)
  `(let ((to (parse-type (nth 0 form))))
      (extract form (source)
        ,@validation
        (append-entry code
          (assign (res code to)
                  (conv ,op source source-type to))))))

(defop truncate
  "Truncate an integer or floating point number.

  (truncate 10 i8) => 10"
  (generic-conversion (if (integer? source-type) "trunc" "fptrunc")
    (cond
      ((not (or (and (integer? source-type) (integer? to))
                (and (float? source-type) (float? to))))
         (bad-input-type form "truncate" "integer" 1 source-type))
      ((> (width to) (width source-type))
         (raise form "Can't truncate '~A' to '~A'. You're looking for (extend)"
           source-type to)))))

(defop sextend
  "Extend an integer preseving the sign."
  (generic-conversion "sext"
    (cond
      ((not (integer? source-type))
         (bad-input-type form "sextend" "integer" 1 source-type))
      ((< (width to) (width source-type))
         (raise form "Can't sextend '~A' to '~A'. You're looking for (truncate)"
           source-type to)))))

(defop extend
  "Extend an integer or floating point number."
  (generic-conversion (if (integer? source-type) "sext" "fpext")
    (cond
      ((not (or (and (integer? source-type) (integer? to))
                (and (float? source-type) (float? to))))
         (bad-input-type form "sextend" "integer" 1 source-type))
      ((< (width to) (width source-type))
         (raise form "Can't extend '~A' to '~A'. You're looking for (truncate)"
           source-type to)))))

(defop ptr->int
  (generic-conversion "ptrtoint"
    (cond
      ((not (pointer? source-type))
        (bad-input-type form "ptr->int" "pointer" 1 source-type))
      ((not (integer? to))
        (bad-input-type form "ptr->int" "integer" 2 to)))))

(defop int->ptr
  (generic-conversion "ptrtoint"
    (cond
      ((not (pointer? source-type))
        (bad-input-type form "int->ptr" "integer" 1 source-type))
      ((not (integer? to))
        (bad-input-type form "int->ptr" "pointer" 2 to)))))

(defop bitcast
  "Converts any object to an integer of equal size.")
(defop coerce
  "Like (bitcast) but it don't give a fuck about size.")

;;; Data structures

(defop type
  (destructuring-bind (name def) form
    (append-entry (define-type name def form code)
      (assign (res code (int 1)) (constant (int 1) "true")))))

(defop tuple
  "Create a tuple from its arguments.

  (tuple 1 2 3) => {1,1,1} with type {i64,i64,i64}
  (tuple \"/usr/bin/ls\" 755) => {\"/usr/bin/ls\",755} with type {i8*,i64}"
  (extract-list form
    (let ((tup-type (aggregate extracted-types)))
      (append-entry code
        (loop for i from 0 to (1- (length extracted-types)) collecting
          (let ((type (nth i extracted-types))
                (last-reg (res code)))
            (assign (res code tup-type)
                    (emit "insertvalue ~A ~A, ~A ~A, ~A" tup-type
                      (if (eql i 0) "undef" last-reg)
                      type (nth i extracted-registers) i))))))))

(defcore nth
  "Access an element on a pointer or an aggregate type.")

(defcore access
  "Access a field on a structure (Or a pointer to one, to any indirection)."
  (let ((field (car form)))
    (extract (cdr form) (obj)
      (cond
        ((not (struct? obj-type)
           (raise form "Can't access a field of a non-structural object.")))
        ((not (member field (names obj-type))))
        (t
          (append-entry code
            (let ((pos (position field (names obj-type))))
              (aif (indirection obj-type)
                ;; It's a pointer, so we have to use gep
                nil
                ;; It's a plain old structure, so we can use extractelement
                (assign (res code (nth pos (types obj-type)))
                  (emit "extractvalue ~A ~A, ~A" obj-type obj pos))))))))))

;; Function definition and calling

(defop function
  (define-function form code))

(defop apply
  "Apply a function pointer to a tuple of arguments.")

(defop call)

;;; Vectors

#|(defop vector
  "Create a vector from its arguments.

  (vector 1 1 1) => <1,1,1> with type <3 x i64>
  (vector 1 3.14) => error
  (vector (vector 1 0 0)
          (vector 0 1 0)
          (vector 0 0 1)) => the identity matrix with type <9 x i64>")

(defop shuffle)|#

;;; Memory

#|(defop allocate)
(defop store)
(defop load)|#

(defop create
  "Allocate an object or an array of objects on the heap.

  (create {i32,i32,i32}) => {i32,i32,i32}*")
(defop reallocate
  "Resize an array.

  (realloc i8* 10) => resizes the array to ten elements")
(defop destroy
  "Frees a pointer.

  (destroy i8*) => true")

(defop defmemman
  "Define a new memory manager.")

(defop address
  "Get the address of a variable or reference.")
(defop fn
  "Get a pointer to a named function.")

;; Software Transactional Memory primitives

(defop transact
  "Takes a list of variables and ensures they are operated on properly.
  See the chapter on STM.")

;;; Printing

(defcore print
  "Generic print function. Prints integers of arbitrary size (Double-dabble)

  When printing integers, the compiler allocates enough space on the stack
  to hold the integer, then casts the pointer to something generic and sends the
  pointer and the integer's size to the double-dabble function. The optimizer
  will remove it from any final version of the code, so this isn't really paying
  for what you're not using.

  When printing floating point numbers, the Grisu3 algorithm is used. Pointers
  are printed in hexadecimal form by first calling (ptr->int).

  Vectors and tuples are printed by concatenating the result of printing their
  elements.

  All print functions are multi-valued: Their first return value is the actual
  string, the second is the length of the string."
  (extract form (val)
    (cond
      ((integer? val-type)))))

;;; FFI

(defop link
  "Link to a foreign library.

  (link \"GL\") => Links to the OpenGL library.

  (link
    (case os
      :windows \"SDL_mixer.dll\"
      :linux \"libSDL_mixer.so\")) => Links to the SDL Mixer library in a
                                      platform-independent way"
  (extract form (lib)
    (append-entry
      (append-toplevel code
        (emit "declare i8 @link(i8*)"))
      (assign (res code (int 8))
        (call "link" :ret (int 8)
                     :args (list (list lib lib-type)))))))

(defop foreign
  "Define a foreign function. The first argument is whether it's from a C or
C++ library. This information is used by Hylas to determine whether to mangle
the name of the function and how to do so.")

;;; LLVM and Assembly

(defop asm)
(defop inline-asm)

(defop LLVM
  "Append a string of LLVM IR to the global scope."
  (let ((asm (nth 0 form)))
    (append-toplevel code
      asm)))

(defop inline-LLVM
  "Append a string of LLVM IR in the current context."
  (let ((asm (nth 0 form)))
    (append-entry code
      asm)))

(defparameter initial-code
  (make-instance '<code>
   :operators *operators*
   :core *core*))

;;; Introspection

(defop declare)

(defop register)
(defop local)
(defop global)

(defop jit)
