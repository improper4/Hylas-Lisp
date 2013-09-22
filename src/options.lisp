(in-package :hylas)
(annot:enable-annot-syntax)

(defclass <option> ()
  ((values :accessor opt-values :initarg :values :initform (list nil))))

(defun opt (default &optional values)
  (if values
      ;; The option has a list of allowed values
      (make-instance '<option> :values values)
      ;; The option is boolean
      (make-instance '<option>)))

#|(defparameter +allowed-options+ (make-hash-table :test #'equal))

(defmacro defopt (name default &optional values)
  `(setf (gethash ,(format nil "~(~A~)" (symbol-name name)) +allowed-options+)
      ,(if values `(opt ,default ,values) `(opt ,default))))|#

(defparameter +allowed-options+
  (hash '|tco| (opt nil)))

(defmethod opt? (opt (code <code>))
  )

(defmethod opt-val (opt (code <code>))
  )

(defmethod opt? (opt val (code <code>))
  )
