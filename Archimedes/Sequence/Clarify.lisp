(defpackage :archimedes.sequence.clarify)

(defun look (x c)
  (cdr (cond
    ((null x) nil)
    ((eql (caar x) c) (car x))
    (t  (look (cdr x) c)))))

(defun range (x);Generate x - 
  (if (eql x 0)
      (cons 0 nil)
      (cons x (range (- x 1)))))
