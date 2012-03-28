(in-package :language-definition)

(setf (readtable-case *readtable*) :invert)

(defparameter plist ())
(defparameter dlist ())
(defparameter stop nil)
(defparameter *counter* 0)
(defmacro out (&rest args)
  `(format t "~a~%" (concatenate 'string ,@args)))

(defun load-language-definition-file (file-name)
  (out "Reading language definition: " file-name)  
  (setf plist ())
  (setf dlist ())
  (setf stop nil)
  (setf *counter* 0)
  (in-package :language-definition) 
  (let ((in (open file-name :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
	 while line do 
	   (add-to-plist (read-from-string (clean line))))
      (close in))
    (if (not in) (out "File doesn't exit" file-name)))
;   (princ plist)
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
	    ((char= c #\Return) #\Space)
	    (t c))
	 cleanedup-line)
	))
    (vector-push-extend #\) cleanedup-line)
    cleanedup-line))

(defun add-to-plist (def) 
  (cond
    ((and (not stop)  def)
     (setf dlist (cons (car def) (cons (cdr def) dlist)))
     (setf plist (cons (car def)
		       (cons (quantify-elements  (cdr def)) plist)))
      ;; (format t "~&~a=~a~%" (car def) (quantify-elements (cdr def)))
     )
    (t (setq stop t))) 
  )

;; (load-language-definition-file "c:\\home\\mysrc\\lisp\\eocs\\jack-compiler\\jack-def-test.txt")
(defun quantify-elements (def)
  (if (find '/ def) (cons '/ (quantify-elements-rec def))
      (quantify-elements-rec def))
  )

(defun quantify-elements-rec (def)
  (cond
   ((null def) nil)
   (t
    (let* ((elt (if (listp (car def))
		    (quantify-elements (car def))
		  (car def)))
	   (quantifier (read-quantifier (cadr def))))
      (if (equal elt '/) (quantify-elements-rec (cdr def))
	  (cons (concatenate 'list (list elt) (or quantifier (list )))
		(quantify-elements-rec (if quantifier
					   (cddr def)
					   (cdr def)))))))))

(defun read-quantifier (elt)
  (cond
    ((equal elt '?) (list 0 1))
    ((equal elt '*) (list 0 most-positive-fixnum))
    ((equal elt '+) (list 1 most-positive-fixnum))
    ((number-listp elt) elt)
    (t nil)))

(defun number-listp (elt)
  (and (listp elt) (numberp (car elt))))

(defun get-toplevel-construct-definition ()
  ;; (print "getting toplevel def")
  ;; (print plist)
   (getf plist 'file))

(defun get-construct-definition (element)
  (incf *counter*)
  (if (> *counter* 5000) (error "to many def requests. Must be a loop!!!"))
  (getf plist element))
(defun get-construct-definition-readable (element)
  (getf dlist element))

(defun identifier (token)
  ;; (format t "Comparing [~a] with [~a]~%" (cdr token) "alpha_")
  ;; (if (equal (cdr token) "alpha_")
  ;;     (format t "Validated by custom function..~%")
  ;;     (format t "Not validated by custom function!!!~%")
  ;;   )
  ;; (print (type-of token))
  ;; (print (type-of "alpha_"))
  (equal (cadr token) "alpha_"))


(defun integerConstant (token)
  (equal (cadr token) "number"))

(defun stringConstant (token)
  (equal (cadr token) "string"))


;(identifier '("bla" . "alpha_"))
;(print ( quantify-elements def)) 
;(defparameter def (list 'class "string" (list 'a '* 'b) (list 'c 'd) '?))
;(print def)
;(load-defs "")
;(print ( listify-definitions definitions))

; (defun get-definition-old (definition)
 ; (getf plist  ( intern definition :language-definition)))

;(print (get-definition  'subroutineDec))

;(print ( getf plist 'statement) )

;(print ( get-toplevel-definition))

;;this doubly recursive function listifies the formal language definition from above , now not needed anymore... 
(defun listify-definitions (defs)
  (cond ((null (car defs)) () )
	(t (cons (labels ((read-def () 
			    (let ((p (pop defs))) 
			      (cond ((equal p '@) ())
				    (t (cons p (read-def)))))))
		   (read-def ))
		 (listify-definitions defs)))))



