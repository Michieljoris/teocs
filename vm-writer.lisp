(in-package :vm-writer)

(setf (readtable-case *readtable*) :invert)
(defparameter class-symbol-table (make-hash-table))
(defparameter subroutine-symbol-table (make-hash-table))
(defparameter classname nil)
(defmacro @class (identifier
		  class-var-declaration-list
		  subroutine-declaration-list)
  (setf classname (eval identifier))
  
  ;; (format t "~&Creating class [~a] with ~a vars and ~a subroutines~%"
  ;; 	  identifier (length class-var-declaration-list)
  ;; 	  (length subroutine-declaration-list))
  
  )

(defmacro @className (identifier)
  `(symbol-name ',identifier)
  )

;; (defun static (identifier-type
;; 	       identifier-list)
;; )

;; (defun field (identifier-type
;; 	      identifier-list)) 

;; (defun constructor (identifier-type-or-void
;; 		    identifier
;; 		    parameter-list
;; 		    subroutine-var-declaration-list
;; 		    statement-list))

;; (defun @function (identifier-type-or-void
;; 		 identifier
;; 		 parameter-list
;; 		 subroutine-var-declaration-list
;; 		 statement-list))

;; (defun method (identifier-type-or-void
;; 	       identifier
;; 	       parameter-list
;; 	       subroutine-var-declaration-list
;; 	       statement-list))

;; (defun parameter (identifier-type
;; 		  identifier))

;; (defun var (identifier-type
;; 	    identifier-list)
;;   )

;; (defun let-statement (identifier
;; 		      expr1
;; 		      expr2)
;;   )

;; (defun while-statement (expr
;; 			statement-list)
;;   )

;; (defun if-statement (expr
;; 		     statement-list1
;; 		     statement-list2)
;;   )

;; (defun do-statement (identifier1
;; 		     identifier2
;; 		     expr-list) ;;classname|varname.subroutinename(exprlist)...
;;   )
       
;; (defun return-statement (expr)
;;   )

;; (defun expr (term op-term-list)
;;   )

;; (defun op-term (op term)
;;        )

;; ;;term:
;; ;integer, string, true, false, null, this
;; ;identifier expr,subroutinecall, expr, unaryop term

;; (defun subroutine-call (identifier1 identifier2 expr-list)
  ;; )

					;difference with do-statement is that it leaves it's return value on the stack, do discards it i think

(defun eval-jack (jack-filename language-definition-filename )
  (let* ((sexpr-string (get-sexpr-string jack-filename language-definition-filename))
	 (sexpr ))
    (in-package :vm-writer)
    (setf sexpr (read-from-string sexpr-string))
    (pprint sexpr)
    ;; (eval sexpr)
    ))

(eval-jack "c:/home/mysrc/lisp/eocs/11/pong/test.jack"
	    "c:/home/mysrc/lisp/eocs/jack-compiler/jack-def-test.txt")