(in-package :tokenizer)
;;primitive DFA looking for numbers, strings, whitespace, signs, keywords and
;;identifiers, discarding comments and comment blocks
;;Every token gets put in its own list, with the cadr set to its type and
;;the white space found before it set as its caddr 
;;A more advanced version would let you specify the tokens to look for in the
;;language grammar definition file.
(defparameter *char-fetcher* nil)
(defparameter *token* nil) 
(defparameter *gobbler* nil)
(defparameter *tokens* (list nil))
(defparameter *current-token* *tokens*)
(defparameter *mark-stack* (list nil))
(defparameter *last-space* "empty space!!")
(defparameter *keywords* (list "void" "if" "class" "constructor"
			       "function" "method" "field" "static"
			       "var" "int" "char" "boolean" "true"
			       "false" "null" "this" "let" "do"
			       "else" "while" "return"))
(defun get-char () (funcall *char-fetcher*)) 

(defun set-char-fetcher-for (pathname)
  (setf *char-fetcher* 
	(let ((in (open pathname :if-does-not-exist nil))
	      (processing t))
	  
	  (if in (format t "Tokenizing ~a~%" pathname))
	  (if in
	      (lambda ()
		(if processing 
		    (let (( return-char (read-char in nil)))
		      (cond ((null return-char)
			     (close in)
			     (setf processing nil)))
		      return-char)
		  nil))
	    (lambda () (print "file doesn't exist!!") nil)))))

(defun load-tokens (file-name)
  (set-char-fetcher-for file-name)
  (setf *token* nil)
  (setf *gobbler* #'reset-gobbler)
  (setf *tokens* (list nil))
  (setf *current-token* *tokens*)
  (setf *mark-stack* (list nil))
  (let (char) (setf char "bla")
       (loop while (setf char (get-char)) do (funcall *gobbler* char)))
  (setf *tokens* (cdr *tokens*))
  (setf *current-token* *tokens*)
  (format t "~{~a~%~}" *tokens*)
  ;; (print-tokens)
  )

(defun reset-gobbler (char) (push-char char) (set-gobbler char))
(defun gobble-alpha-chars (char)
  (cond ( (not (or (char= char #\_) (alpha-char-p char)))
	  (set-gobbler char)  (pop-token "alpha_")))
  (push-char char))
(defun gobble-digits (char)
  (cond (( not (digit-char-p char))
	 (set-gobbler char) (pop-token "integerConstant")))
  (push-char char))
(defun gobble-white-space (char)
  (cond ((not ( white-space-p char))
	 (set-gobbler char) (pop-token "space")))
  (if (char= char #\Return) ()
      (push-char char)))
(defun gobble-return (char)
  (cond (( not (char= #\Return char))
	 (set-gobbler char)
	 (pop-char)
	 (push-char char))))
(defun gobble-string (char)
  (cond ((char= char #\")
	 (push-char char) (pop-token "stringConstant")
	 (setf *gobbler* #'reset-gobbler))
	(t (push-char char))))

(defun gobble-sign (char)
  (pop-token "sign") 
  (reset-gobbler char))
(defun gobble-sign-or-comment (char)
  (cond ((char= char #\*)
	 (pop-char)
	 (setf *gobbler*
	       (get-gobble-comment-block-function)))
	(t (cond ((char= char #\/)
		  (pop-char)
		  (setf *gobbler* #'gobble-comment-line))
		 (t (gobble-sign char))))))
(defun gobble-comment-line (char)
  (if (char= char #\Newline) (setf *gobbler* #'reset-gobbler)))
(defun get-gobble-comment-block-function ()
  (let ((prev-char #\a))
    (lambda (char)
      (if (and (char= char #\/)
	       (equal prev-char #\*))
	  (setf *gobbler* #'reset-gobbler))
      (setf prev-char char))))
(defun set-gobbler (char)
  (setf *gobbler*
	( cond ((alpha-char-p char) #'gobble-alpha-chars)
	       ((digit-char-p char) #'gobble-digits)
	       ((white-space-p char) #'gobble-white-space)
	       ((char= char #\") #'gobble-string)
	       ((char= char #\/) #'gobble-sign-or-comment)
	       ((char= char #\Return) #'gobble-return)
	       (t #'gobble-sign-or-comment))))

(defun white-space-p (char) 
   (or (char= char #\Newline)  (char= char #\Space)
	      (char= char #\Tab) (char= char #\Linefeed)))

(defun is-keyword (alpha)
  (find alpha *keywords* :test #'equal))

(defun pop-token (type)
  ;; (format t "<~a> ~a~%" type (string (coerce ( reverse *token*) 'string)) )
  (let ((token (string (coerce ( reverse *token*) 'string))))
    ;; (format t "New token is: [~a]~%" token)
    (setf type 
	  (if (equal type "alpha_")
	      (if (is-keyword token) "keyword"
		"identifier")
	    type))
    (if (equal type "space")
	(setf *last-space* (string (coerce (reverse *token*) 'string)))
      (let ((new-token (list  (list 
			       token
			       type
			       *last-space*))))
	(setf *last-space* "")
	(setf (cdr *current-token*) new-token)
	(setf *current-token* new-token))))
  (setf *token* nil))

(defun push-char (char) ( setf *token* (cons char *token*)))
(defun pop-char () (setf *token* (cdr *token*)))

(defun get-token ()
  (let ((return-token (car *current-token*)))
    (setf *current-token* (cdr *current-token*))
    ;; (format t "next token = ~a~%" return-token)
    return-token))
(defun set-mark ()
  (push *current-token* *mark-stack*)
  ;; (format t "~&@n~a:~% " (caar *current-token*))
  (caar *current-token*)
  )

(defun go-back-to-last-set-mark ()
  (setf *current-token* (car *mark-stack*))
  ;; (format t "~&@<~a:~% " (caar *current-token*))
  )
(defun discard-last-set-mark ()
  (let ((discarded-mark (car ( pop *mark-stack*))))
    (declare (ignore discarded-mark))
    ;; (format t "Forgetting mark on token ~a~%" discarded-mark)
    ))

;; (load-tokens "c:/home/EOCS/projects/Pong/Main.jack")


(defun print-tokens ()
  (dolist (i *tokens*)
    (format t "~a~a" (caddr  i) (car i))))


