;Exhaustive depth first recursive search EBNF programmable LL parser
;producing AST in sexpr form
(in-package :ast-builder)
(setf (readtable-case *readtable*) :invert)
(defparameter stack nil)
(defparameter *matched-tokens* (list nil))
(defparameter *current-token* *matched-tokens*)
(defparameter *mark-stack* (list nil))

(defun build-ast (jack-filename language-definition-filename)
  (load-tokens jack-filename)
  (load-language-definition-file language-definition-filename)
  (init-terminal-generator)
  (format t "Starting processing -------------------------------~%")
  (format t "~a~%" (compare-and-iterate  (get-terminal t) (get-token)))
  (format t "~&Finished -----------------------------------------~%"))

(build-ast "c:/home/mysrc/lisp/eocs/test.jack"
	     "c:/home/mysrc/lisp/eocs/jack-compiler/jack-def-test.txt")
(defun compare-and-iterate (terminal token)
  (format t "comparing: terminal [~a] and token [~a]~%" terminal (car token))
  (cond
    ((and terminal (not token)) "ERROR: Unexpected EOF")
    ((and (not terminal) token) "ERROR: EOF expected")
    ((not (or token terminal)) "Success!!")
    (t (compare-and-iterate
	(get-terminal (match terminal token)) (get-token)))))

(defun get-terminal (last-match)
  (if (null stack) 
      (format t "Stack is empty. No valid parse found...")
      (cond
	(last-match
	 (cond ((listp last-match) ;gather up tokens that matched
		(let ((m (list last-match)))
		  (setf (cdr *current-token*) m)
		  (setf *current-token* m))
		))
	 (cond
	   ((null (car stack))
	    (format t "Top of stack is empty. Found a valid parse~%")
	    (pop stack)
	    nil)
	   (t
	    (expand-node)
	    (let ((terminal (pop (car stack))))
	      (pop (car stack))
	      (format t "next terminal = ~a~%" terminal)
	      terminal))))
	 (t
	  (format t "Discarding path: ~%~a~%" (pop stack))
	  (cut-branches)
	  (go-back-to-last-set-marks)
	  (get-terminal t)))))

(defun cut-branches ()
  (cond ((equal '@ (caar stack)) ;this is a node,
	 (pop stack)   ;discarded all paths, so discarding node
	 (discard-last-set-marks)
	 (cut-branches)))
  
  )

(defun match (terminal token)
  (format t "matching [~a] with [~a]~%"  terminal (car token))
  (if (stringp terminal) (progn
			   (format t "string compare result: ~a~%"
				   (equal terminal (car token)))
			   (if (equal terminal (car token))
			       token))
      
      ;; (if (symbolp terminal)
      
      (let ((sym-name (symbol-name terminal)))
	    (cond
	      ((string-equal sym-name "stringConstant")
	       (stringConstant token))
	      ((string-equal sym-name "identifier")
	       (identifier token))
	      ((string-equal sym-name "integerConstant")
	       (integerConstant token))
	      (t (error "Don't know this terminal: ~a~%" terminal))))))
;; (handler-case (funcall terminal token)
;; 		  (undefined-function
;; 		   () (format t "Error: undefined construct ~a~%" terminal)))))  


(defun init-terminal-generator ()
  (setf stack nil)
  (setf *matched-tokens* (list nil))
  (setf *current-token* *matched-tokens*)
  (setf *mark-stack* (list nil))
  (set-marks)
  (push (get-toplevel-construct-definition) stack)
 )

(defun factor-out-regex (elt q)
  (let ((sn (symbol-name q)))
    (cond
      ((equal sn "?") (list (list '/  elt '!  nil '!) '!)  )
      ((equal sn "*")  (list ( list '/ (list elt '! elt '*) '! nil '!) '!)  )
      ((equal sn "+")  (list elt '! elt '*))
      (t (list elt '!)))))

(defun unpack-top-node ()
  (let* ((top (pop stack))
	 (node (factor-out-regex (pop top) (pop top))))
    (push (concatenate 'list node top) stack)))

(defun expand-node ()
  (unpack-top-node)
  (format t ">>>foctored out the regex ? and + and * of the first elt:~%")
  (print-stack-top)
  (let ((node (caar stack)))
    (cond
      ((listp node)
       (cond
	 ((equal (car node) '/)
	  (let ((top (car stack)))
	    (push (set-marks) (car stack)) 
	    (push '@ (car stack)) ;mark it as a node
	    ;(cond ((car stack) (push nil stack) (set-marks)))
	    (grow-branches (cdr node) (cddr top))
	    (format t ">>>Grew branches:~%")
	    (print-stack)
	    (expand-node)))
	 (t
	  (let ((prod (pop stack)))
	    (push (concatenate 'list (car prod) (cddr prod)) stack)
	    (format t ">>>Unwrapped list [~a]:~%" (car prod))
	    (print-stack-top)
	    (expand-node))
	  )))
      ((not (stringp (caar stack)))
       (let ((def (get-construct-definition (caar stack))))
	 (if def
	     (let ((prod (pop stack)))
	       (push (concatenate 'list def (cddr prod)) stack)
	       (format t ">>>Expanded prodname [~a]:~%" (car prod))
	       (print-stack-top)
	       (expand-node))))))))

(defun grow-branches (branches rest-of-branch)
  (cond
    ((null branches) nil)
    (t (let ((elt (pop branches))
	     (q (pop branches)))
	 (if elt
	     (if  (and (listp elt) (equal q '!) (not (equal '/ (car elt))))
		  (push (concatenate 'list elt rest-of-branch) stack)
		  (push (cons elt (cons q rest-of-branch)) stack))
	     (push rest-of-branch stack))
	 (grow-branches branches rest-of-branch)))))
    

(defun start ()
  (init-terminal-generator)
  (print-stack))


(defun set-marks ()
  (push *current-token* *mark-stack*)
  (set-mark)
  )

(defun go-back-to-last-set-marks ()
  (go-back-to-last-set-mark) 
  (setf *current-token* (car *mark-stack*))
  )

(defun discard-last-set-marks ()
  (discard-last-set-mark)
  (let ((discarded-mark (car ( pop *mark-stack*))))
    (declare (ignore discarded-mark))
    ))

(defun print-tokens ()
  (dolist (i (cdr *matched-tokens*))
    (format t "~a~a" (caddr  i) (car i))))


(defun identifier (token)
 ;;  (format t "Comparing [~a] with [~a]~%" (cdr token) "alpha_")
 ;;  (if (equal (cdr token) "alpha_")
 ;;      (format t "Validated by custom function..~%")
 ;;      (format t "Not validated by custom function!!!~%")
 ;;    )
 ;;  (print (type-of token))
 ;;  (print (type-of "alpha_"))
 ;; (format t "type of token is :~a~%" (cadr token))
 (if  (equal (cadr token) "alpha_")  token))

(defun integerConstant (token)
  (if  (equal (cadr token) "number") token))

(defun stringConstant (token)
  (if  (equal (cadr token) "string") token))
;----------------------------------------------------


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

(defun print-stack-top ()
  (labels ((p (production)
	     (cond
	       ((null production) nil)
	       (t (format t " ~a~a "   (car production) (cadr production))
		  (p (cddr production))))))
    (p (car stack))
    (format t "~%")))
