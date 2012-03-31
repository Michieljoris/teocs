(in-package :ast-builder)
(setf (readtable-case *readtable*) :invert)

(defun build-ast (jack-filename language-definition-filename)
  (walk-tree jack-filename language-definition-filename)
  )

(defun convert-to-sexp ()
    (read-from-string (with-output-to-string (*standard-output*)
				(print-tokens) )))


(defun print-tokens ()
  (dolist (i (cdr *matched-tokens*))
    (let ((elt (car i))
	  (type (cadr i))
	  (space (caddr i)))
      (format t "~a ~a ~a~%" elt type (type-of elt)))))

(defun print-tokens1 ()
  (dolist (i (cdr *matched-tokens*))
    (let ((elt (car i))
	  (type (cadr i))
	  (space (caddr i)))
      (if (not (symbolp elt)) (setf space " ") )
      (if (symbolp elt) (setf elt (symbol-name elt))
	(if (equal type "sign") (setf elt "")
	  (if (not ( equal type "ast")) 
	    (setf elt (format nil "-~a-" elt)))))
      
      (format t "~a~a" space elt))))


(defun print-stack ()
  )
(defun print-stack2 ()
  (let ((indent 1)
	(index 0)
	(stack (reverse stack)))
    (dotimes (i (length stack))
      (format t "~VT~a: " indent index )
      (cond ((equal (car (elt stack i)) '@)
	     (incf indent)
	     ;(pop (elt stack i))
	     ))
      (incf index)
      (labels ((p (production)
		  (cond
		   ((null production) nil)
		   (t (format t " ~a~a "   (car production) (cadr production))
		      (p (cddr production))))))
	(p (elt stack i))
	(format t "~%")))))
(defun print-stack-top ())
(defun print-stack-top1 ()
  (labels ((p (production)
	     (cond
	       ((null production) nil)
	       (t (format t " ~a~a "   (car production) (cadr production))
		  (p (cddr production))))))
    (p (car stack))
    (format t "~%")))



(build-ast "c:/home/mysrc/lisp/eocs/11/Pong/Main.jack"
	     "c:/home/mysrc/lisp/eocs/jack-compiler/jack-def-test.txt")