(in-package :hylas-test)
(annot:enable-annot-syntax)

#|(defparameter +args+
  (list (int 64) (generic :a)
        (make-instance '<func> :ret (int 1) :args (list (generic :b)
                                                           (generic :b)))
        (pointer (generic :c)))
  "((a i64) (b generic-a) (c (fn bool generic-b generic-b))
   (d (pointer generic-c)))")

@doc "These argument lists should not match against `+args+`"
(defparameter +invalid-arg-lists+
  (list
    (list (int 32))))

@doc "These arg lists should match `+args+`"
(defparameter +valid-arg-lists+
  (list
    (list (int 64) +double+)))|#

; (def-suite basic
;   :description "firing up a parser, setting input to something, etc.")
; (in-suite basic)

; (test initialization
;   (is (with-parser t)))

; (test string-input
;   (is (with-string-input ("derp") t)))

(defparameter args
  (mapcar #'(lambda (type) (parse-type type initial-code))
    '(|i64| (|?| |Ta| |i32| |single|))))

(run!)
