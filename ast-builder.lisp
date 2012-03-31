;Exhaustive depth first recursive search EBNF programmable LL parser
;producing AST in sexpr form
(in-package :ast-builder)
(setf (readtable-case *readtable*) :invert)

(defstruct ast
  ast sign keyword integerConstant stringConstant )

(defun build-ast (jack-filename language-definition-filename)
  (print-tokens (walk-tree jack-filename language-definition-filename)))

(defun convert-to-sexp ()
    (read-from-string (with-output-to-string (*standard-output*)
				(build-ast) )))


(defun print-tokens (matched-tokens)
  (dolist (i (cdr matched-tokens))
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


(build-ast "c:/home/mysrc/lisp/eocs/11/Pong/Main.jack"
	     "c:/home/mysrc/lisp/eocs/jack-compiler/jack-def-test.txt")