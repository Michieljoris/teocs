(in-package :ast-builder)

(setf (readtable-case *readtable*) :invert)

(defun build-ast (jack-filename language-definition-filename)
  (load-tokens jack-filename)
  (load-language-definition-file language-definition-filename)
  (format t "Starting processing -------------------------------~%")
  ( process-list-of-constructs (get-toplevel-construct-definition))
  (format t "Finished -----------------------------------------~%")
  )

(build-ast "c:/home/mysrc/lisp/eocs/11/Pong/Ball.jack"
 	   "c:/home/mysrc/lisp/eocs/jack-compiler/jack-def-test.txt")

(defun process-construct (construct)
  ;; (format t "Processing construct ~a~%" construct)
  (let ((unquantified-construct (pop construct))
	(min (pop construct)))
    (if (null min) (progn
		     ;; (format t "About to processing unquantified construct ~a~%" unquantified-construct)
		     (let ((result (process-unquantified-construct unquantified-construct)))
		       ;; (format t "process-unquantified  result = ~a~%" result)
		       result))
      (let ((max (pop construct)))
	(set-mark)
	;; (format t "-->Expecting ~a between ~a and ~a times.~%" unquantified-construct min max)
	(let ((count (dotimes (count max count)
		       (if (or (>= count max)
			       (not (process-unquantified-construct
				     unquantified-construct)))
			   (return count)))))
	  (cond
	   ((< count min)
	    ;; (format t "~&****Fail. Could only match [~a] ~a times. Found [~a] instead~%"
	    ;; 	    unquantified-construct count (car (get-token)))
	    (go-back-to-last-set-mark)
	    nil)
	   (t
	    ;; (format t "--------->Success. Found ~a ~a times~%" unquantified-construct count)
	    (discard-last-set-mark)
	    t)))))))

(defun process-unquantified-construct (unquantified-construct)      
  (set-mark)
  ;; (format t "Expecting to gobbkle unquantified [~a] one time only~%" unquantified-construct)
  (cond
   ((gobble-unquantified-construct unquantified-construct)
    ;; (format t "gobbled ~a~%" unquantified-construct)
    (discard-last-set-mark)
    t)
   (t
    (go-back-to-last-set-mark)
    ;; (format t "Failed to gobble ~a~%" unquantified-construct)
    nil)))

(defun gobble-unquantified-construct (unquantified-construct)
  (cond
   ((listp unquantified-construct)
    (process-list-of-constructs unquantified-construct))
   ((symbolp unquantified-construct)
    (let ((construct (get-construct-definition unquantified-construct)))
      ;; (format t "Fetched definition of: ~a --> ~a~%"
      ;; 	      unquantified-construct
      ;; 	      (get-construct-definition-readable unquantified-construct))
      (cond
       ((null construct)
	(let* ((token (get-token))
	      (result (handler-case (funcall unquantified-construct token)
			     (undefined-function
			      ()
			      (progn (format t "Error: undefined construct ~a~%" token)
				     nil)))))
	  (format t "~a " (car token))
	  result))
       (t
	(process-list-of-constructs construct)))))
   (t       ;  literal
    (let ((terminal (car (get-token))))
      (if (null terminal) (error "Reached the end of the jack file!!!"))
      (let ((result  (equal unquantified-construct terminal)))
	(if result (format t "~a " unquantified-construct))
	result)))))

(defun process-list-of-constructs (list-of-constructs)
  ;; (format t "~&READING list of constructs= ~a~%" list-of-constructs)
  (if (equal '/ (car list-of-constructs))
      (process-or-list-of-constucts (cdr list-of-constructs))
    (process-and-list-of-constructs list-of-constructs)))

(defun process-and-list-of-constructs (list-of-constructs)
  ;; (format t "Processing list of and constructs ~a~%" list-of-constructs)
  (let* ((optional-stack nil)
	(result 
	 (dolist (construct list-of-constructs t)  ; sequential list
	   ;; (if (equal 0 (cadr construct))
	   ;;     (format t  "Storing ~a on optional-stack~%" construct)
	   ;;   (format t  "Expecting  ~a~%" construct))
	   (if (equal 0 (cadr construct))
	       (push construct optional-stack)
	     (if (process-construct construct)
		 (setf optional-stack nil)
	       (progn  ;construct has failed, but let's try with optionals in front...
		 ;; (format t "construct has failed~%")
		 ;; (if (> (length optional-stack) 0)
		 ;;     (format t "CHECKING SKIPPED OPTIONALS"))
		 (setf optional-stack (reverse optional-stack))
		 (if (dolist (opt optional-stack t)
		       (pop optional-stack)
		       (if (try-multiple-optional opt construct) (return nil))
		       ;; (if (process-construct opt)
		       ;; 	  (if (process-construct construct) (return t)))
		       )
		     (return nil))
		       ))))))
    (when (and result (> (length optional-stack) 0))
      ;; (format t "TRYING LEFT OVER ~a OPTIONALS~%" (length optional-stack))
      (setf optional-stack (reverse optional-stack))
      (dolist (opt optional-stack)
	(process-construct opt))  
      ;; (format t "pr-and-list returns ~a~%" result)
      )
    result 
    ))

(defun try-multiple-optional (opt construct)
  (let ((unquantified-optional (car opt))
	(max (caddr opt)))
    (if (equal max 1)
	(and (process-construct opt) (process-construct construct) )
      (process-and-list-of-constructs
       (list (list unquantified-optional)
	     (list unquantified-optional 0 (- max 1)) construct)))))

(defun process-or-list-of-constucts (list-of-constructs) 
  ;; (format t "Processing list of or defs ~a~%" deflist)
  (dolist (construct list-of-constructs nil)
    (if (process-construct construct) (return t))))


 