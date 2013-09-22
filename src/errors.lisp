(in-package :hylas)

(define-condition hylas-error (error)
  ((msg :initarg :msg :reader msg))
  (:report (lambda (condition stream) (format stream "~A" (msg condition)))))

(defmacro raise (code msg &rest args)
  `(error 'hylas-error
    :msg (format nil ,(concatenate 'string msg "~%Code state:~%~a")
           ,@args ,code)))

(defun bad-input-type (code name expected-type pos given-type)
  (error code "(~A) expected ~A as its ~A argument, but got ~A"
    expected-type pos given-type))
