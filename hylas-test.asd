(defsystem hylas-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:hylas
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "hylas")
                 (:file "pat")
                 (:file "fn")
                 (:file "lambda"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
