(in-package :hylas)
(annot:enable-annot-syntax)

(defclass <memman> ()
  ((create :accessor create-fn :initarg :create)
   (destroy :accessor destroy-fn :initarg :destroy)
   (reallocate :accessor realloc-fn :initarg :realloc)
   (gc-stat :accessor gc-stat-fn :initarg :gc-stat)
   (pause-gc :accessor gc-pause-fn :initarg :gc-pause-fn)
   (start-gc :accessor gc-start-fn :initarg :gc-start-fn)))

(defparameter +memman-protos+
  nil)
