(defmacro aif (test true-branch &optional false-branch)
  `(let ((it ,test))
     (if it ,true-branch ,false-branch)))
