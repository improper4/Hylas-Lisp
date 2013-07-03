(in-package :hylas)

(defmacro raise (code msg &rest args)
  `(error (format nil ,(concatenate 'string msg "~%Code state:~%~a") ,@args ,code)))
