;This package walks the grammar tree, using the tokens as its guide.
;It tries to find a way to the end of a branch.
(in-package :tree-walker)
(setf (readtable-case *readtable*) :invert)
(defparameter stack nil)
(defparameter *matched-tokens* (list nil))
(defparameter *current-token* *matched-tokens*)
(defparameter *mark-stack* (list nil))
(defparameter newline (string (coerce '(#\Newline) 'string)))

(defun walk-tree (jack-filename language-definition-filename)
  (load-tokens jack-filename)
  (load-language-definition-file language-definition-filename)
  (init-terminal-generator)
  (format t "Analyzing..~%")
  (format t "~a~%" (compare-and-iterate  (get-terminal t) (get-token)))
  *matched-tokens*
  )

(defun compare-and-iterate (terminal token)
  (cond
    ((and terminal (not token)) "ERROR: Unexpected EOF")
    ((and (not terminal) token) "ERROR: EOF expected")
    ((not (or token terminal)) "Finished walking the tree")
    (t (compare-and-iterate
	(get-terminal (match terminal token)) (get-token)))))

(defun get-terminal (last-match)
  (if (null stack) 
      (format t "Stack is empty. No valid parse found...~&")
      (cond
	(last-match
	 (cond ((listp last-match) ;found token, tokens come in lists...
	 	(record-match last-match))) ;write it away into our log
	 (cond
	   ((null (car stack)) ;got to the end of the branch..
	    (format t "Top of stack is empty. Found a valid parse~%")
	    (pop stack)
	   ;to find other parses, do a get-terminal here.. 
	    nil)
	   (t  ;still processing branch.. 
	    (expand-node)
	    (let ((terminal (pop (car stack))))
	      (pop (car stack)) ;get rid of superfluous regexp marker
	     ;the expand-node above would have factored them out by now 
	      (cond ;before we send this terminal back to be compared
	       ;to the next token, check if it is an ast marker,
	       ((is-ast-marker terminal)
		; make a note of it in our log of matched tokens
		(record-match (list  terminal "ast" (pop (car stack))))
		;and send the next terminal to be compared instead
		(get-terminal t))
	       (t ;this terminal is not an ast marker, send it to be compared
		terminal))))))
	(t ;terminal didn't match token, abandon these branches..
	  (pop stack)
	  (cut-branches)
	  (go-back-to-last-set-marks)
	  (get-terminal t)))))

(defun is-ast-marker (sym)
  (and (symbolp sym) ( equal #\$ (elt (symbol-name sym) 0))))

(defun cut-branches ()
  (cond ((equal '@ (caar stack)) ;this is a node,
	 (pop stack)   ;discarded all paths, so discarding node
	 (discard-last-set-marks)
	 ;do this recursively, a branch might be the last branch of
	 ;it's parent branch, it has to be cut too then.
	 (cut-branches))))

(defun match (terminal token)
  (if (stringp terminal) (progn
			  ;found a real terminal, has to match token 
			   (if (equal terminal (car token))
			       token))
    ;our terminal is a symbol, is our token what the symbol stands for?
    ;Find out by comparing it to the type of the token.
    (if ( string-equal (symbol-name terminal) (cadr token)) token)))

(defun init-terminal-generator ()
  (setf stack nil)
  (setf *matched-tokens* (list nil))
  (setf *current-token* *matched-tokens*)
  (setf *mark-stack* (list nil))
  (set-marks)
  (push (cons '@ (get-toplevel-construct-definition)) stack)
  (push (cdar stack) stack)
 )

(defun factor-out-regex (elt q)
  (let ((sn (symbol-name q)))
    (cond
      ((equal sn "?")
       (list (list '/  elt '! (list '$open '! '@list  '$close '! '@list) '!) '! ))
      ((equal sn "*")
       (list '$open '! '@list ( list '/ (list elt '! elt '&) '! nil '!) '!
	     '$close '! '@list)  )
      ((equal sn "&")
       (list  (list '/ (list elt '! elt '&) '! nil '!) '!))
      ((equal sn "+")  (list elt '! elt '*))
      (t (list elt '!)))))

(defun unpack-top-node ()
  (let* ((top (pop stack))
	 (node (factor-out-regex (pop top) (pop top))))
    (push (concatenate 'list node top) stack)))

(defun expand-node ()
  (unpack-top-node)
  (let ((node (caar stack)))
    (cond
      ((listp node)
       (cond
	 ((equal (car node) '/)
	  (let ((top (car stack)))
	    (push (set-marks) (car stack)) 
	    (push '@ (car stack)) ;mark it as a node
	    (grow-branches (cdr node) (cddr top))
	    (expand-node)))
	 (t
	  (let ((prod (pop stack)))
	    (push (concatenate 'list (car prod) (cddr prod)) stack)
	    (expand-node))
	  )))
      ((not (stringp (caar stack)))
       (let ((def (get-construct-definition (caar stack))))
	 (if def
	     (let ((prod (pop stack)))
	       (push (concatenate 'list
				  (list '$open  '! (car prod))
				  def
				  (list'$close  '! (car prod))
				  (cddr prod)) stack)
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

(defun record-match (match)
  (let ((m (list match)))
    (setf (cdr *current-token*) m)
    (setf *current-token* m)))

(defun print-stack ()
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



;; (walk-tree "c:/home/mysrc/lisp/eocs/11/pong/test.jack"
;; 	   "c:/home/mysrc/lisp/eocs/jack-compiler/jack-def-test.txt" )
