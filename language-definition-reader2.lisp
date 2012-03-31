(in-package :language-definition)

(setf (readtable-case *readtable*) :invert)

(defparameter production-list ())
(defparameter dlist ())
(defparameter stop nil)
(defparameter *counter* 0)
(defmacro out (&rest args)
  `(format t "~a~%" (concatenate 'string ,@args)))

(defun load-language-definition-file (file-name)
  (out "Reading language definition: " file-name)  
  (setf production-list ())
  (setf dlist ())
  (setf stop nil)
  (setf *counter* 0)
  (in-package :language-definition) 
  (let ((in (open file-name :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
	 while line do 
	   (add-to-production-list (read-from-string (clean line))))
      (close in))
    (if (not in) (out "Language grammare file doesn't exit" file-name)))
;   (princ production-list)
  )

(defun clean (line)
  (let ((cleanedup-line (make-array 5
				    :fill-pointer 0
				    :adjustable t
				    :element-type 'character))
	(in-quotes nil))
    (vector-push-extend #\( cleanedup-line)
    (dotimes (i (length line))
      (let ((c (elt line i)))
	(vector-push-extend  
	  (cond
	    ((char= c #\')
	     (setf in-quotes (not in-quotes))
	     #\")
	    (in-quotes c) 
	    ((or (char= c #\?) (char= c #\*) (char= c #\+))
	     (vector-push-extend #\Space cleanedup-line)
	     c)
	    ((char= c #\|) #\/)
	    ((char= c #\:) #\Space)
	    ((char= c #\;) #\Space) ;productions have to finish with a
	    ;semicolon, but for now they are discarded, productions
	    ;have to fit on a line, the EOL is the marker to read a new
	    ;production.This does not conform to the grammar definition
	    ;of grammar definitions, which states that productions are
	    ;ended with semicolons. 
	    ((char= c #\Return) #\Space)
	    (t c))
	 cleanedup-line)
	))
    (vector-push-extend #\) cleanedup-line)
    ;(format t "~a~%" cleanedup-line)
    cleanedup-line
    ))

(defun add-to-production-list (def) 
  (cond
    ((and (not stop)  def)
     (setf dlist (cons (car def) (cons (cdr def) dlist)))
     (let ((production (quantify-elements  (cdr def))))
       (if (equal (car production) '/)
	   (setf production (list production '!)))
       (setf production-list (cons (car def)
				   (cons production production-list)))
       ;(format t "~&~a = ~{~s ~}~%" (car def) production)
       ))
    (t (setq stop t))) 
  )

;; (load-language-definition-file "c:\\home\\mysrc\\lisp\\eocs\\jack-compiler\\jack-def-test.txt")
(defun quantify-elements (def)
  (if (find '/ def) (cons '/ (quantify-elements-recursive def))
      (quantify-elements-recursive def))
  )

(defun quantify-elements-recursive (def)
  (cond
   ((null def) nil)
   (t
    (let* ((elt (if (listp (car def))
		    (quantify-elements (car def))
		  (car def)))
	   (quantifier (read-quantifier (cadr def))))
      (if (equal elt '/) (quantify-elements-recursive (cdr def))
	(cons  elt ( cons  quantifier (quantify-elements-recursive
				       (if (equal '! quantifier)
					   (cdr def)
					 (cddr def))))))))))

(defun read-quantifier (elt)
  (if (not (or (equal elt '?)
	       (equal elt '*)
	       (equal elt '+)
	       (number-listp elt)))
      '!
    elt
    )
  )

(defun read-quantifier-old (elt)
  (cond
    ((equal elt '?) (list 0 1))
    ((equal elt '*) (list 0 most-positive-fixnum))
    ((equal elt '+) (list 1 most-positive-fixnum))
    ((number-listp elt) elt)
    (t nil)))

(defun number-listp (elt)
  (and (listp elt) (numberp (car elt))))

(defun get-toplevel-construct-definition ()
   (getf production-list 'file))

(defun get-construct-definition (element)
  (incf *counter*)
  (if (> *counter* 8000) (error "to many def requests. Must be a loop!!!"))
  (getf production-list element))
(defun get-construct-definition-readable (element)
  (getf dlist element))
