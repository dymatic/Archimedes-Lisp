(defun drop (x)
  "Drop the last element of a list"
  (cons (car x) (cond
		  ((not (cddr x)) nil)
		  (t (drop (cdr x))))))
