(defpackage :archimedes.sequence.manipulate
  (:use :archimedes.sequence.common :archimedes.sequence.functional))

(defun removeA (x c)
  (cond
    ((null x) nil)
    ((eql (car x) c) (removeA (cdr x) c))
    (t (cons (car x) (removeA (cdr x) c)))))

(defun elemp (x c)
  (cond
    ((null x) nil)
    ((eql (car x) c) t)
    (t (elemp (cdr x) c))))

(defun pos (x c)
  (if (elemp x c)
      (length (applyBreak (lambda (y) (not (eq y c))) x))
      -1))

(defun sub (x c)
  (cond
    ((eql c 0) x)
    ((null x) x)
    (t (sub (cdr x) (- c 1)))))

(defun to (x c)
  (cons (car x) (if (eql c 0)
	      nil
	      (drop (to (cdr x) (- c 1))))))

(defun after (x y)
 (cdr (sub x (pos x y))))

(defun before (x y)
  (to x (pos x y)))

(defun zip (x c)
  (if (or (null x) (null c)) 
      nil
      (cons (list (car x) (car c)) (zip (cdr x) (cdr c)))))

(defun positions (x c)
  (removeA (mapcar (lambda (y) (if (eql (car y) c) (cadr y) nil)) (zip x (reverse (range (length x))))) nil))

