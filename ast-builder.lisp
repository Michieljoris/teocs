;Exhaustive depth first recursive search EBNF programmable LL parser
;producing AST in sexpr form
(in-package :ast-builder)
(setf (readtable-case *readtable*) :invert)

(defmacro make-ast-format (&rest r)
  (let ((res nil))
    (dolist (e r)
      (push (list
	     'cons
	     (symbol-name (car e))
	     (concatenate 'list (list 'lambda (list 'elt 'arg) ) (cdr e)))
	    res))
    (read-from-string (format nil "~s" (cons 'list res)))))

(defparameter newline (string (coerce '(#\Newline) 'string)))
(defparameter source-format
  (make-ast-format
   ( ast  (declare (ignore elt)) (declare (ignore arg)) "")
   ( sign (concatenate 'string arg elt )) 
   ( keyword (concatenate 'string arg elt )) 
   ( operand (concatenate 'string arg elt )) 
   ( qualifier (concatenate 'string arg elt )) 
   ( integerConstant (concatenate 'string arg elt ))
   ( identifier (concatenate 'string arg elt ))
   ( stringConstant (concatenate 'string arg elt ))))

(defparameter xml-format
  (make-ast-format
   ( ast (if (not (string-equal arg "@list"))
	     (if (string-equal (symbol-name elt) "$open")
		 (concatenate 'string "<" (symbol-name arg) ">" newline)
	       (if (string-equal (symbol-name elt) "$close") 
		   (concatenate 'string "</" (symbol-name arg) ">" newline)))
	   ""))
   ( sign (declare (ignore arg)) (concatenate 'string
		       "<symbol>" elt "</symbol>" newline)) 
   ( operand (declare (ignore arg)) (concatenate 'string
		       "<symbol>" elt "</symbol>" newline)) 
   ( keyword (declare (ignore arg)) (concatenate 'string
		       "<keyword>" elt "</keyword>" newline)) 
   ( qualifier (declare (ignore arg)) (concatenate 'string
		       "<keyword>" elt "</keyword>" newline)) 
   ( identifier (declare (ignore arg)) (concatenate 'string
		       "<identifier>" elt "</identifier>" newline)) 
   ( integerconstant (declare (ignore arg)) (concatenate 'string
		       "<integerConstant>" elt "</integerConstant>" newline))
   ( stringConstant (declare (ignore arg)) (concatenate 'string
		       "<stringConstant>" elt "</stringConstant>" newline)))) 

(defparameter type-format
  (make-ast-format
   ( ast (if (not (string-equal arg "@list"))
	     (if (string-equal (symbol-name elt) "$open")
		 (concatenate 'string "ast<" (symbol-name arg) ">" (typeof elt) newline)
	       "")))
   ( sign (declare (ignore arg)) (concatenate 'string
		       "<symbol>" elt  (typeof elt) newline)) 
   ( operand (declare (ignore arg)) (concatenate 'string
		       "<symbol>" elt  (typeof elt) newline)) 
   ( keyword (declare (ignore arg)) (concatenate 'string
		       "<keyword>" elt  (typeof elt) newline)) 
   ( qualifier (declare (ignore arg)) (concatenate 'string
		       "<keyword>" elt  (typeof elt) newline)) 
   ( identifier (declare (ignore arg)) (concatenate 'string
		       "<identifier>" elt  (typeof elt) newline)) 
   ( integerconstant (declare (ignore arg)) (concatenate 'string
		       "<integerConstant>" elt  (typeof elt) newline))
   ( stringConstant (declare (ignore arg)) (concatenate 'string
		       "<stringConstant>" elt (typeof elt) newline)))) 

(defun typeof (sym)
 (cond
  ((stringp sym) " string")
  ((symbolp sym) " symbol")
  (t "???????????")
  ))

(defparameter sexpr-format
  (make-ast-format
   ( ast  (if (string-equal arg "@list")
   	      (if (string-equal (symbol-name elt) "$open") " (list " ") " )
	    (if (string-equal (symbol-name elt) "$open")
		(concatenate 'string
			     " (@" (invert-case2 (symbol-name arg)) " ") ") ")))
   ( sign (declare (ignore elt)) (declare (ignore arg)) "") 
   ( operand (declare (ignore arg)) (concatenate  'string "\"" elt "\" "))
   ( keyword  (declare (ignore elt)) (declare (ignore arg)) "") 
   ( qualifier (declare (ignore arg)) (concatenate 'string  "'" elt " ")) 
   ( integerConstant (declare (ignore arg)) (concatenate 'string elt " "))
   ( identifier (declare (ignore arg)) (concatenate 'string "'$" elt " "))
   ( stringConstant (declare (ignore arg)) (concatenate 'string elt " "))))

;;hacking to get around CL's annoying case handling of symbols. 
(defun invert-case (s)
  (flet ((uppercasep (c) (and (char>= c #\A) (char<= c #\Z)) )
	 (lowercasep (c) (and (char>= c #\a) (char<= c #\z)) ) )
    (let ((has-lowercase nil)
	  (has-uppercase nil))
      (if (dotimes (i (length s) nil)
	    (let ((c (elt s i)))
	      (cond
	       ((uppercasep c)
		(if has-lowercase (return t))
		(setf has-uppercase t))
	       ((lowercasep c)
		(if has-uppercase (return t))
		(setf has-lowercase t)))))
	  s
	(if has-lowercase (string-upcase s)
	  (if has-uppercase (string-downcase s) s))))))

;more efficient one..
(defun invert-case2 (s)
  (let ((gobble-char nil)
	(string-case nil))
    (labels ((uppercasep (c) (and (char>= c #\A) (char<= c #\Z)) )
	     (lowercasep (c) (and (char>= c #\a) (char<= c #\z)) )
	     (start-gobbling (c) (cond
				   ((lowercasep c)
				     (setf string-case 'lower)
				     (setf gobble-char
					   (lambda (c) (uppercasep c))))
				   (t
				    (cond ((uppercasep c)
					   (setf string-case 'upper)
					   (setf gobble-char
						 (lambda (c) (lowercasep c)))))))
			      nil))
      (setf gobble-char #'start-gobbling)
      (if (dotimes (i (length s) nil)
	    (if (funcall gobble-char (elt s i)) (return t)))
	  s 
	(case string-case
	  (upper (string-downcase s))
	  (lower (string-upcase s))
	  (otherwise s))))))

(defun build-ast (jack-filename
		  language-definition-filename
		  ast-format
		  output-stream)
  (let ((matched-tokens
	 (walk-tree jack-filename language-definition-filename)))
    (dolist (i (cdr matched-tokens))
      (let ((elt (car i))
	    (type (cadr i))
	    (arg (caddr i)))
	(format t "~a"
		(funcall
		 (get-formatter type source-format) elt arg))))
    (format t "~&")
    (dolist (i (cdr matched-tokens))
      (let ((elt (car i))
	    (type (cadr i))
	    (arg (caddr i)))
	(format output-stream "~a"
		(funcall
		 (get-formatter type ast-format) elt arg))))))

(defun get-formatter (type ast-format)
  (cdr (assoc type ast-format :test #'string-equal)))

(defun get-sexpr-string (jack-filename language-definition-filename )
  ; (read-from-string
   (with-output-to-string
     (output)
     (build-ast jack-filename
		language-definition-filename
		sexpr-format
		output)))
;; ) 

;; (defparameter se (build-sexpr
;; 		  "c:/home/mysrc/lisp/eocs/11/pong/Ball.jack"
;; 		  "c:/home/mysrc/lisp/eocs/jack-compiler/jack-def-test.txt"))
;; (pprint (read-from-string se ) )



;; (defun get-ast (jack-filename language-definition-filename ast-type)
;;    (with-output-to-string
;;      (output)
;;      (build-ast jack-filename
;; 		language-definition-filename
;; 		sexpr-format
;; 		output))))

;; (build-ast "c:/home/mysrc/lisp/eocs/11/pong/test.jack"
;; 	   "c:/home/mysrc/lisp/eocs/jack-compiler/jack-def-test.txt"
;; 	   type-format
;; 	   *standard-output*)
