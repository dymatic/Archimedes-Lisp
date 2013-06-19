; Functions used in applying functions over lists
(defun applyBreak (f x)
  (if (null x)
      nil
      (if (funcall f (car x))
	  (cons (car x) (applyBreak f (cdr x)))
	  nil)))

(defun removeBreak (f x)
  (cond
    ((null x) nil)
    (t (if (funcall f (car x))
	   (cdr x) 
	   (removeBreak f (cdr x))))))
