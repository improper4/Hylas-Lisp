(defpackage hylas-test
  (:use :cl :hylas :fiveam))
(in-package :hylas-test)

(def-suite context
  :description "Context-related stuff.")
(in-suite context)

(test scope-context
  (is
    (eq
      (let ((code initial-code))
        (with-new-scope code
          (last-context code)))
      :normal))
  (is
    (eq
      (let ((code initial-code))
        (with-new-scope code
          (with-function-scope code
            (last-context code))))
      :fn))
  (is
    (eq
      (let ((code initial-code))
        (with-new-scope code
          (with-lambda-scope code
            (last-context code))))
      :lambda)))

(run!)
