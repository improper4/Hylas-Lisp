;;; # Pattern Matcher
;;; Pattern matching is used to implement parametric polymorphism in Hylas. For example, a generic function might have the following prototype:
;;;
;;; ((a i64) (b generic-a) (c (fn bool generic-b generic-b)) (d (pointer generic-c)))
;;;
;;; Here, only the first argument is specific. The second can be any type, the fourth can be any pointer type (With indirection => 1), and the third and most complex is a pointer to any function that returns a boolean and takes any two arguments that must be of the same type. This kind of complexity in generic argument lists requries a pattern matcher, whose usefulness extends beyond generic functions as it can be used to pattern match fully specialized functions and make the process of calling functions more generic.

(in-package :hylas)
(annot:enable-annot-syntax)

(defclass <binding> ()
  ((name :accessor name :initarg :name)
   (type :accessor binding-type :initarg :type)))

@doc "Turn a list of bindings into a list of lists."
(defun unroll-bindings (list)
  (loop for binding in list collecting
    (list (name binding) (binding-type binding))))

(defun match-arg (proto call)
  (let ((type-proto (type-of proto))
        (type-call (type-of call)))
    (if (eq type-proto '<generic-type>)
        (make-instance '<binding> :name proto :type type-call)
        (when (eq type-proto type-call)
              (case type-proto
                (<integer>
                  (eql (width proto) (width call)))
                (<func>
                  (list (match-arg (ret proto) (ret call))
                    (loop for i from 0 to (1- (length (args proto))) collecting
                      (match-arg (nth i (args proto)) (nth i (args call))))))
                (<aggregate>
                  (loop for i from 0 to (1- (length (types proto))) collecting
                    (match-arg (nth i (types proto)) (nth i (types call)))))
                (<scalar>
                  (equal (scalar-type proto) (scalar-type call))))))))

@doc "See if the arg lists `first` and `second` match. Optionally, indicate what
type of variable-arity `first` is (:varargs uses LLVM varargs, while
(:rest [type]) uses Hylas array varargs.

`first` should be considered the arg list from a function prototype, while
`second` should be considered the arg list from a function call."
(defun pat-match (first second &optional varargs)
  (when (or (eql (length first) (length second)) varargs)
        (let ((res (append
                     (loop for i from 0 to (1- (length first)) collecting
                       (match-arg (nth i first) (nth i second)))
                     (when varargs
                       (append
                        (list :varargs)
                        (loop for i from (1- (length first))
                          to (1- (length second))
                          collecting (nth i second)))))))
          (if (member nil res)
              nil
              (aif
                (unroll-bindings
                  (remove-if #'(lambda (x) (not (typep x '<binding>)))
                    (alexandria:flatten res)))
                it t)))))
