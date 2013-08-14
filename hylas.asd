(defclass makefile (source-file) ())

(defmethod source-file-type ((c makefile) (s module)) nil)

(defmethod output-files ((operation compile-op) (f makefile))
  (values
    (list
      (make-pathname :name "Makefile"
                     :type nil
                     :defaults (component-pathname f))) t))

(defmethod perform ((o load-op) (c makefile)) t)

(defmethod perform ((o compile-op) (c makefile))
  (unless (zerop (run-shell-command "make"))
    (error 'operation-error :component c :operation o)))

(defsystem hylas
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:trivial-utf-8
               :cl-annot
               :iterate
               :cffi
               :optima
               :cl-gendoc)
  :serial t
  :components ((:file "package")
               (makefile "Makefile")
               (:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "packages")
                 (:file "hylas")
                 (:file "errors")
                 (:file "reader")
                 (:file "types")
                 (:file "pat")
                 (:file "core")
                 (:file "fn")
                 (:file "llvm")
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
