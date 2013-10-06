(in-package :hylas-test)

(def-suite fn-tests
  :description "Test parsing functions, function definition, etc..")
(in-suite fn-tests)

#|(test parse-fn-failure
  (signals
    (error 'hylas-error)
    (parse-function '())
    (parse-function '(name))
    (parse-function '(name ()))
    (parse-function '(name ((n) (m i64))))
    (parse-function '(name ((n) (m i64)) ret))
    (parse-function '(name ((n i64) (m i64)) ret))))

(test parse-fn-success
  (is (parse-function '(name ((n i64) (m i64)) ret code))
      (list 'name '(n m) '(i64 i64) ret "" (list 'code))))|#

(test concrete-functions
  (finishes
    (eval-string
      "(function ((n i64)) i64 n)")
    (eval-string
      "(function ((n double) (m double)) double
         (+ n m))")))

(run!)
