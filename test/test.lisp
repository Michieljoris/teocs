
(defun rev (l)
  (cond
    ((null l) ())
    (t (append (rev (cdr l)) (list (car l))))))

(print (rev (list 'a 'b 'c)))
