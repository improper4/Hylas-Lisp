#|
  This file is a part of hylas project.
  Copyright (c) 2013 Fernando Borretti (eudoxiahp@gmail.com)
|#

(in-package :cl-user)
(defpackage hylas-test-asd
  (:use :cl :asdf))
(in-package :hylas-test-asd)

(defsystem hylas-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:hylas
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "hylas"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
