#|
  This file is a part of hylas project.
  Copyright (c) 2013 Fernando Borretti (eudoxiahp@gmail.com)
|#

#|
  Author: Fernando Borretti (eudoxiahp@gmail.com)
|#

(in-package :cl-user)
(defpackage hylas-asd
  (:use :cl :asdf))
(in-package :hylas-asd)

(defsystem hylas
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:c2ffi-cffi)
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "hylas")
                 (:file "types")
                 (:file "pat")
                 ;(:file "core")
                 (:file "fn")
                 (:file "jit"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op hylas-test))))
