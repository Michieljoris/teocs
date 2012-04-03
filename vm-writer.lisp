(in-package :vm-writer)

(setf (readtable-case *readtable*) :invert)
;; (defparameter class-symbol-table (make-hash-table :test #'equal))
(defparameter class-symbol-table nil)
;; (defparameter subroutine-symbol-table (make-hash-table :test #'equal))
(defparameter subroutine-symbol-table nil)
(defparameter classname nil)
(defparameter index 0)
(defparameter sub-index 0)
;; (defparameter field "field")
;; (defparameter static "static")
;; (defparameter int "int")
;; (defparameter boolean "boolean")

(defun init ()
  (setf index 0)
  (setf class-symbol-table nil)
  )

(defun @class (identifier
		  class-var-declaration-list
		  subroutine-declaration-list)
  (setf classname identifier)
  ;; (dolist )
  (format t "~&creating class [~a] with ~a vars and ~a subroutines~%"
  	  identifier (length class-var-declaration-list)
  	  (length subroutine-declaration-list))
  
  )

(defun @className (identifier) identifier )
(defun @type (identifier-type) identifier-type) 
(defun @varname (identifier) identifier)
(defun @subroutineName (identifier) identifier)

(defun @classVarDec (kind type varname varnamelist)
  (push (list kind type index) class-symbol-table)
  (push varname class-symbol-table)
  (incf index)
  (dolist (varname varnamelist)
    (push (list kind type index) class-symbol-table)
    (push varname class-symbol-table)
    (incf index)))
(defun @subroutineDec (subroutineType returnType subroutineName
		       parameterList subroutineBody)
  
  )
(defun @parameterList (parameterType parameterName parameterList)
  (setf sub-index 0)
  (setf subroutine-symbol-table nil)
  (push parameterName parameterList)
  (push parameterType parameterList)
  (do () ((null parameterList))
    (push (list 'argument (pop parameterList) sub-index) subroutine-symbol-table)
    (push (pop parameterList) subroutine-symbol-table)
    (incf sub-index))
  (setf sub-index 0))

(defun @subroutineBody (varList statementList)
  )

(defun @varDec (varType varName varList)
  (push varName varList)
  (do () ((null varList))
    (push (list 'var varType sub-index) subroutine-symbol-table)
    (push (pop varList) subroutine-symbol-table)
    (incf sub-index)))
  
(defun @statement (statement))
(defun @doStatement (subroutineCall))
(defun @classCall (varName subroutineName expressionList))
(defun @expressionList (expression expressionList))
(defun @expression (term optermList))
(defun @term (term) term)
(defun @op (operand))
(defun @returnStatement (expression))
(defun @letStatement (varName expressionList expression))
(defun @ifStatement (expression statementList elseList))
(defun @whileStatement (expression statementList))
(defun @arrayReference (varname expression))
(defun @parensedExpression (expression))
(defun @unaryOppedTerm (unaryOp term))
(defun @localCall (subroutineName expressionList))
(defun @unaryOp (unaryOp))
(defun @keywordConstant (keywordConstant))
(defun @op (operand))
;; ;;term:

;; ;integer, string, true, false, null, this
;; ;identifier expr,subroutinecall, expr, unaryop term

					;difference with do-statement is that it leaves it's return value on the stack, do discards it i think

(defun eval-jack (jack-filename language-definition-filename )
  (let* ((sexpr-string (get-sexpr-string jack-filename language-definition-filename))
	 (sexpr ))
    (in-package :vm-writer)
    (setf sexpr (read-from-string sexpr-string))
    (pprint sexpr)
    (init)
     (eval sexpr)
    (format t "~&class symbol table :~%~{ ~a ~a~%~}" class-symbol-table)
    (format t "~&subroutine symbol table :~%~{ ~a ~a~%~}" subroutine-symbol-table)
    ))

(eval-jack "c:/home/mysrc/lisp/eocs/11/Seven/Main.jack"
	    "c:/home/mysrc/lisp/eocs/jack-compiler/jack-def-test.txt")
