(defsystem hylas-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:hylas
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "hylas")
                 (:file "pat"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
