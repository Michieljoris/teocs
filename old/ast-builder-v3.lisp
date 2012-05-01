;Exhaustive depth first recursive search EBNF programmable LL parser
;producing AST in sexpr form
(in-package :ast-builder)
(setf (readtable-case *readtable*) :invert)
(defparameter *defstack* nil)
(defparameter *opt-stack* nil)

(defun build-ast (jack-filename language-definition-filename)
  (load-tokens jack-filename)
  (load-language-definition-file language-definition-filename)
  (init-terminal-generator)
  (format t "Starting processing -------------------------------~%")
  (format t "~a~%" (iterate (get-terminal t) (get-token)))
  (format t "~&Finished -----------------------------------------~%"))

(build-ast "c:/home/mysrc/lisp/eocs/11/Pong/Main.jack"
	     "c:/home/mysrc/lisp/eocs/jack-compiler/jack-def-test.txt")
;; (defun test ()
;;   (build-ast "c:/home/mysrc/lisp/eocs/11/Pong/Main.jack"
;; 	     "c:/home/mysrc/lisp/eocs/jack-compiler/jack-def-test.txt"))
(defun iterate (terminal token)
  (format t "iterating: terminal ~a token ~a~%" terminal token)
  (cond
    ((and terminal (not token)) "ERROR: Unexpected EOF")
    ((and (not terminal) token) "ERROR: EOF expected")
    ((not (or token terminal)) "Success!!")
    (t (if (match terminal token)
	   (iterate (get-terminal t) (get-token))
	   (iterate (get-terminal nil) token)))))

(defun get-terminal (last-terminal-matched)
  (format t "last terminal matched? ~a~%" last-terminal-matched)
  (cond
    (last-terminal-matched
     (setf *opt-stack* nil)
     (get-first-non-optional-terminal (pop *defstack*)))
    (t
     nil 
;;try optionals
     )))

(defun test ()
  (let ((continue t))
    (load-language-definition-file
     "c:/home/mysrc/lisp/eocs/jack-compiler/jack-def-test.txt")
    (init-terminal-generator)
    (loop while continue do
	  (setf continue (get-first-non-optional-terminal (pop *defstack*)))
	  (format t "~a" continue))))
(test)
;; (defun find-terminal (construct-list)
;;   (format t "Finding terminal in ~a~%" construct-list)
;;   (let ((terminal (get-first-non-optional-terminal construct-list)))
;;     (or terminal
;; 	(if (> (length *defstack*) 0)
;; 	    (find-terminal (pop *defstack*))
;; 	  nil))))


(defun match (terminal token)
  (format t "matching [~a] with [~a]~%" (car  terminal) (car token))
  (if (stringp (car terminal)) (progn
				 (format t "string compare result: ~a~%"
					 (equal (car terminal) (car token)))
				 (equal (car terminal) (car token)))
      (handler-case (funcall (car terminal) token)
	(undefined-function ()
	  (format t "Error: undefined construct ~a~%" terminal)))))  

(defun init-terminal-generator ()
  (setf *defstack* nil)
  (push (get-toplevel-construct-definition) *defstack*))

(defun get-first-non-optional-terminal (construct-list)
  "gets first non-optional terminal in a construct list or nil
  Store optionals encountered on the opt-stack
  If a list or symbol is encountered, travel down the tree
  ,putting partially processed lists on the def-stack"
  (let ((mandatory-construct (skip-and-store-optionals construct-list)))
    (if mandatory-construct
	(let ((unquantified-construct (caar mandatory-construct)))
	  (if (cdr mandatory-construct)
	      (push (cdr mandatory-construct) *defstack*))
	  (cond
	   ((listp unquantified-construct)   ;embedded construct list
	    (get-first-non-optional-terminal unquantified-construct)) 
	   ((stringp unquantified-construct) (car mandatory-construct)) ;terminal: literal
	   (t (let ((construct-list (get-construct-definition
				     unquantified-construct)))
		(if construct-list (get-first-non-optional-terminal construct-list)
		  (car mandatory-construct))))))
      (if (> (length *defstack*) 0)
	  (get-first-non-optional-terminal (pop *defstack*))))))


(defun skip-and-store-optionals (construct-list)
  (cond
    ((equal 0 (cadar construct-list))
     (push (car construct-list) *opt-stack*)
     (skip-and-store-optionals (cdr construct-list)))
    (t construct-list)))



 
