(in-package :hylas)
(annot:enable-annot-syntax)

@document "Low-level code generation and LLVM interface."

@doc "This function destructively modifies the `code` object that is passed as
its argument, so it should only be used within a call to `append-entry`. It
returns a copy of the (updated) object, so it can be queried as usual for things
like the last register value."
(defun emit-code (form code &optional &key (in-lambda nil))
  (format t "Reading form: ~A" form)
  (if (atom form)
    (cond
      ((eq '|true| form)
        (append-entry code
          (assign-res (int 1) (constant (int 1) "true"))))
      ((eq '|false| form)
        (append-entry code
          (assign-res (int 1) (constant (int 1) "false"))))
      ((integerp form)
        (append-entry code
          (assign-res (int 64) (constant (int 64) form))))
      ((floatp form)
        (append-entry code
          (assign-res +double+ (constant +double+ form))))
      ((stringp form)
        (let ((bytes (trivial-utf-8:string-to-utf-8-bytes form)))
          (append-entry
            (append-toplevel code
              (assign
                (new-string code)
                (format nil "global [~A x i8] c[~{\\~A~}\\00]" (length bytes)
                  (loop for char across bytes collecting
                    (write-to-string char :base 16)))))
            (assign (res code +cstr+) (memload +cstr+ (current-string code))))))
      ((symbolp form)
        (multiple-value-bind (var pos) (lookup form code)
          (if var
              (progn
                ;; If we're in a lambda, check whether the symbol comes
                ;; from a lexical context other than the local or global
                ;; ones
                (append-entry code
                  (memload (var-type var) (emit-var var pos))))
              (raise form "Unbound symbol")))))
    (let ((fn (car form)))
      ;; Input is a list
      (aif (operator? fn code)
        (funcall it (cdr form) code)
        ;;No? Well, user-defined function then
        (aif (callfn fn (cdr form) code)
             it
             ;; Not that? Then it's part of the normal core
             (if (integer-constructor? fn code)
                 (construct-integer (cdr form) code)
                 (aif (core? fn code)
                      (funcall it (cdr form))
                      ;; Since everything above failed, signal an error
                      (raise form "No such function"))))))))

@doc "Takes a form. Produces global IR"
(defun compile-code (form code)
  (let ((out (emit-code form code)))
    (list out
          (format nil "~{~A~&~}~&define ~A @entry(){~&~{    ~A~&~}~&    ret ~A ~A~&}"
                  (toplevel out) (res-type out) (entry out) (res-type out) (res out)))))

@doc "Takes a string, tries to compile it. Output format:
fail
error-type: Normal Error
error-message: ...
Alternatively:
success
[output of the 'print' part of the repl]
"
(defun jit (form code)
  (compile-code form code))

(defun repl (code)
  (loop (princ (jit (safe-read) code))))

(defmacro with-preserved-case (&rest code)
  `(unwind-protect
     (progn
       (setf (readtable-case *readtable*) :preserve)
       ,@code)
     (setf (readtable-case *readtable*) :upcase)))

(defun eval-string (str)
  (with-preserved-case
    (jit (safe-read-from-string str) initial-code)))
