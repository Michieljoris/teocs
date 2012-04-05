(in-package :compilation-engine)

(setf (readtable-case *readtable*) :invert)
(defparameter class-symbol-table nil)
(defparameter subroutine-symbol-table nil)
(defparameter classname nil)
(defparameter subroutine-name nil)
(defparameter subroutine-type nil)
(defparameter index 0)
(defparameter index-static 0)
(defparameter $ (string (coerce '(#\Newline) 'string)))
(defparameter operands '(( "+" . "add") ( "-" . "sub")
			 ( "*" . "Math.multiply 2") ( "/" . "Math.divide 2")
			 ( "=" . "eq") ( "<" . "lt") ( ">" . "gt")
			 ( "&" . "and")  ( "|" . "or") ))
(defparameter label-counter 0)

(defun init ()
  (setf index-static 0)
  (setf label-counter 0)
  )

(defun @class (identifier
		  class-var-declaration-list
		  subroutine-declaration-list)
  (declare (ignore identifier))
  (resetcmd)
  (format t "~&creating class [~a] with ~a varDec lines and ~a subroutines~%"
  	  classname (length class-var-declaration-list)
  	  (length subroutine-declaration-list))
  (dolist (subroutine subroutine-declaration-list (cmd))
    (cmd subroutine)))
(defun @classVarDec (kind type varname varnamelist)
  (push varname varnamelist)
  (do () ((null varnamelist))
    (if (eql kind 'field)
      (push (list 'this type index) class-symbol-table)
      (push (list 'static type index-static) class-symbol-table))
    (push (pop varnamelist) class-symbol-table)
    (if (eql kind 'field) (incf index) (incf index-static))))
(defun @classNameDec (identifier) (setf classname identifier))
(defun @className (identifier) identifier )
(defun @type (identifier-type) identifier-type) 
(defun @varName (identifier) identifier)

(defun @subroutineDec (subroutineType returnType subroutineName
		       parameterList subroutineBody)
  (declare (ignore returnType))
  (declare (ignore parameterList))
  (resetcmd)
  (cmd "function" (format nil "~a.~a" classname  subroutineName) index $)
  (case subroutineType
    (method
     (cmd "push argument 0" $
	  "pop pointer 0" $) )
    (constructor
    ;????????????????? 
     )
    (otherwise ))
  (cmd subroutineBody))
(defun @subroutineTypeDec (type)
  (setf index 0)
  (setf subroutine-symbol-table nil)
  type)
(defun @subroutineName (identifier) identifier)
(defun @parameterList (parameterType parameterName parameterList)
  (push parameterName parameterList)
  (push parameterType parameterList)
  (let ((index 0))
    (do () ((null parameterList))
      (push (list 'argument (pop parameterList) index) subroutine-symbol-table)
      (push (pop parameterList) subroutine-symbol-table)
      (incf index))))

(defun @subroutineBody (varDecs statements)
  (declare (ignore varDecs))
  (resetcmd)
  (cmd statements))
(defun @varDec (varType varName varList)
  (push varName varList)
  (do () ((null varList))
    (push (list 'local varType index) subroutine-symbol-table)
    (push (pop varList) subroutine-symbol-table)
    (incf index)))
  
(defun @statements (statements)
  (resetcmd)
  (dolist (s statements (cmd)) (cmd s))) 
(defun @statement (statement) statement)
(defun @doStatement (subroutineCall)
  (resetcmd)
  (cmd subroutineCall)
  (cmd "pop temp 0" $))
(defun @letStatement (varName index-expression sign expression)
  (declare (ignore sign))
  (resetcmd)
  (cond
   (index-expression
    (cmd index-expression 
	 "pop" (kind-of varName) (index-of varName) $
	 "add" $
	 expression 
	 "pop temp 0" $
	 "pop pointer 1" $
	 "push temp 0" $
	 "push that 0" $)) 
   (t
    (cmd expression)
    (cmd "pop" (kind-of varName) (index-of varName) $))))

(defun @ifStatement (expression statements else-statements)
  (resetcmd)
  (let ((then (unique-label))
	(after-then (unique-label)))
			   (cmd expression 
				"if-goto" then $)
			   (if else-statements
			       (cmd else-statements))
			   (cmd "goto" after-then $
				"label" then $
				 statements 
				 "label" after-then $)))
(defun @whileStatement (expression statements)
  (resetcmd)
  (let ((before-expression (unique-label))
	(after-while (unique-label)))
    (cmd "label" before-expression $
	 expression 
	 "not" $ 
	 "if-goto" after-while $
	 statements 
	 "goto" before-expression $
	 "label" after-while $)))
(defun @returnStatement (expression)
  (resetcmd)
  (if expression
      (cmd expression)
    (cmd "push constant 0" $))
  (cmd "return" $))

(defun @expressionList (expression expressionList)
  (resetcmd)
  (cmd expression)
  (dolist (e expressionList (cmd)) (cmd e)))
(defun @expression (term optermList)
  (resetcmd)
  (cmd term)
  (do ((op optermList (cddr op) cmd)
       (term (cdr optermList) (cddr term)))
      ((null op) (cmd))
    (cmd (car term) 
	 (car op))))

(defun @op (operand)
  (resetcmd)
  (cmd (cdr (assoc operand operands :test #'equal)) $))

;term factors
(defun @term (term) term)
(defun @integer (integer)
  (resetcmd)
  (cmd "push constant" integer $))
(defun @string (string)
  (resetcmd)
  (cmd "push" "constant" (length string) $
       "call String.new 1" $)
  (dotimes (i (length string) (cmd))
    (cmd "push constant" (char-code (elt string i)) $
	 "call String.appendChar 2" $)))
(defun @keywordConstant (keywordConstant)
  (resetcmd)
  (case keywordConstant
    (true (cmd "push constant 0" $
	       "not" $))
    (false (cmd "push constant 0" ))
    (otherwise (cmd "ERRROORRRR keywordconstant is null or this!!" $))))
(defun @variable (varName index-expression)
  (resetcmd)
  (cond
   (index-expression
    (cmd  index-expression
	  "push" (kind-of varName) (index-of varName) $ 
	  "add" $
	  "pop pointer 1" $ 
	  "push that 0" $))
   (t
    (cmd "push" (kind-of varName) (index-of varName) $ ))))

(defun @localCall (subroutineName expressionList)
  (resetcmd)
  ;?????????????
  )
(defun @classCall (varName subroutineName expressionList)
  (resetcmd)
  ;???????????????????????
  )

(defun @parensedExpression (expression) expression)
(defparameter gather nil)
(defun @unaryOppedTerm (unaryOp term)
  (resetcmd)
  (cmd term 
       unaryOp))

(defun @unaryOp (unaryOp)
  (resetcmd)
  (if (equal unaryOp "~") (cmd "not" $)
    (if (equal unaryOp "-") (cmd "neg" $))))
;---------------------------------------------------
;support functions)
(defun kind-of (identifier)
  (symbol-name (car (lookup identifier))))
(defun typeof (identifier)
  (symbol-name (cadr (lookup identifier))))
(defun index-of (identifier)
  (caddr (lookup identifier)))
(defun lookup (identifier)
  (or (getf subroutine-symbol-table identifier)
      (getf class-symbol-table identifier)))
(defun unique-label ()
  (format nil "label-~a" (incf label-counter)))
(defun resetcmd () (setf gather nil))
(defun cmd (&rest l)
  (setf gather (concatenate 'list gather (splice l))))
(defun splice (l)
  (cond ((null l) nil )
	(t (concatenate 'list
			(if (listp (car l)) (car l) (list (car l)))
			(splice (cdr l))))))
(defun print-vm ()
  (format t "VM:~%~{ ~a~}" gather))
;------------------------------------------
;
(defun eval-jack (jack-filename language-definition-filename )
  (let* ((sexpr-string
	  (get-sexpr-string jack-filename language-definition-filename))
	 (sexpr ))
    (in-package :compilation-engine)
    (setf sexpr (read-from-string sexpr-string))
    (pprint sexpr)
    (init)
    (eval sexpr)
    (format t "~&class symbol table :~%~{ ~a ~a~%~}" class-symbol-table)
    (format t "~&subroutine symbol table :~%~{ ~a ~a~%~}"
	    subroutine-symbol-table)
    (print-vm)))

;; (eval-jack "c:/home/mysrc/lisp/eocs/11/Pong/test.jack"
;; 	    "c:/home/mysrc/lisp/eocs/jack-compiler/jack-def-test.txt")



