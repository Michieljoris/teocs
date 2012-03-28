(defpackage :com.michieljoris.jc.vm-writer
  (:nicknames :vm-writer)
  (:uses )
  (:export ))

(in-package :vm-writer)

(setf (readtable-case *readtable*) :invert)
(defparameter *indent* 0)
(defun process_class (identifier
		      class-var-declaration-list
		      subroutine-declaration-list)
  (build-xml 'class
   ('identifier identifier)
   ('symbol '{)
   class-var-declaration-list
   ('symbol '})
  )

(defun static (identifier-type
	       identifier-list)
  (build-xml 'classVarDec
	     'keyword 'stati
	     'keyword identifier-type
	     )
  
(defmacro build-xml (maintag @rest r)
  `(progn
     (open-tag ,maintag)
     (let ((*indent* (+ *indent* 2)))
       ,(list r)
     (close-tag ,maintag))
  ))


(defun field (identifier-type
	      identifier-list)) 

(defun constructor (identifier-type-or-void
		    identifier
		    parameter-list
		    subroutine-var-declaration-list
		    statement-list))

(defun function (identifier-type-or-void
		 identifier
		 parameter-list
		 subroutine-var-declaration-list
		 statement-list))

(defun method (identifier-type-or-void
	       identifier
	       parameter-list
	       subroutine-var-declaration-list
	       statement-list))

(defun parameter (identifier-type
		  identifier))

(defun var (identifier-type
	    identifier-list)
  )

(defun let-statement (identifier
		      expr
		      expr)
  )

(defun while-statement (expr
			statement-list)
  )

(defun if-statement (expr
		     statement-list
		     statement-list)
  )

(defun do-statement (identifier
		     identifier
		     expr-list) ;;classname|varname.subroutinename(exprlist)...
  )
       
(defun return-statement (expr)
  )

(defun expr (term op-term-list)
  )

(defun op-term (op term)
       )

;;term:
;integer, string, true, false, null, this
;identifier expr,subroutinecall, expr, unaryop term

(defun subroutine-call (identifier identifier expr-list) ());difference with do-statement is that it leaves it's return value on the stack, do discards it i think


(progn
  (defparameter a "(c_class Main { function void main ( ) { var squaregame game |;| } })")
  (setf a ( read-from-string a))
  (format t "~%~a" (macroexpand-1 a)))

(defun process (list-of-functions)
  (mapcar #'eval (mapcar #'quote-args list-of-functions)))

(defun open-tag (tag)
 (format t "~VT<~a>~%" *indent* tag))

(defun close-tag (tag)
 (format t "~VT</~a>~%" *indent* tag))

(defun tag (tag name)
 (format t "~VT<~a> ~a </~a>~%" *indent* tag name tag))

