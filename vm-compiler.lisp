(in-package :vm-compiler)
;sbcl --eval '(progn (print *posix-argv*)(quit))' 
;sb-ext:save-lisp-and-die "exename" :executable t
;;; TODO:,get main function working
;;;       init code,
(defparameter out-to-console ())
(defparameter *file-name* "")
(defparameter *label-number* 0)
(defparameter *function-name* "global_function")
(defparameter *segrefs* (list 'argument "ARG" 'local "LCL" 'this "THIS" 'that "THAT"
			       'pointer "R3" 'temp "R5" ))
(defparameter registers (list 'LCL 'ARG 'THIS 'THAT))

(defmacro out (&rest args)
  `(dolist ( a (list
		,@(mapcar #'(lambda (x) (if (atom x) ( list 'quote x) (cons 'list x))) args)))
     (if (atom a) (format t "~a~%" a) (format t "~{~a~}~%" a))))

(defmacro make-label (&rest a) `(list  ,@a ))

(defun init ()
 ; (print "START!!!!!!!!!!!!!")
  ) 

;;read file or dir name from command line and process one file or all .vm files in dir
(defun compile-vm (vmfiles pathname-out)
    (print "Combining the following files and compiling them to asm:")
    (format t "~%~{~a~%~}~%" vmfiles)
    (with-open-file (file-out 
		     pathname-out
		     :direction :output
		     :if-exists :supersede)
      (let ((*standard-output* (if out-to-console *standard-output* file-out)))
	(init)
	(dolist (file vmfiles) 
	  (process-file file))))
   ; (print (if out-to-console "Finished" (concatenate 'string "Asm file written to: " file-out-name ".asm")))
    )

;;process one file
(defun process-file (file-name) 
  ;; read in the file line by line, turn them into sexp and evaluate them
  (let ((in (open file-name :if-does-not-exist nil))
	(*file-name* file-name))
    (when in
      (loop for line = (read-line in nil)
	 while line do 
	   ( process-line line))
      (close in))))

(defun process-line (line)
  ;;strip opening spaces, comments and return if result is an empty line
  ;;append instruction with c_ to prevent clashes with common-lisp names
  (let* ((line (strip line))
	(cmd (read-from-string (concatenate 'string "(c_" line ")"))))
    (setf cmd (cons (car cmd) (mapcar #'quote-it (cdr cmd))))
    ;(format t "~a~a~%" "//--------" line)
    (eval cmd)))
(defun quote-it (x) (list 'quote x))
(defun strip (line)
  (let* ((p1 (or ( position-if #'(lambda (c) (char/= c #\Space)) line) 0))
	(p2 (position-if #'(lambda (c) (char= c #\/)) line :start p1)))
    (subseq line p1 p2)))

;;define all commands. Let them print the machine instructions to standard output
(defun c_ () ) ;;for empty line
(defun c_push (segment index)
  (resolve-segment segment index)
  (out  (( if (eq segment 'constant) 'D=A 'D=M)) @SP A=M M=D @SP M=M+1))
(defun c_pop (segment index)
  (resolve-segment segment index)
  (out D=A @R13 M=D @SP AM=M-1 D=M @R13 A=M M=D))
(defun resolve-segment (segment index)
  (case segment
    (static (out ('@ *file-name* "." index)))
    (constant (out ('@ index))) 
    (otherwise (out ('@ index) D=A ('@ (getf *segrefs* segment))  
		    ( (if (or (eq segment 'pointer) (eq segment 'temp))
			  'A=D+A 'A=D+M))))))

(defun c_not () (out @SP A=M-1 M=!M))
(defun c_neg () (out @SP A=M-1 M=-M))
(defun c_add () (binary-operator "+"))
(defun c_sub () (binary-operator "-"))
(defun c_and () (binary-operator "&"))
(defun c_or () (binary-operator  "|"))
(defun binary-operator (operator)
  (out @SP AM=M-1 D=M A=A-1 ('M=M operator 'D)))

(defun c_eq () (comparison-operator 'JEQ))
(defun c_gt () (comparison-operator 'JGT))
(defun c_lt () (comparison-operator 'JLT))
(defun comparison-operator (operator)
  (let ((is-true (unique-label))
	(end (unique-label)))
    (out @SP AM=M-1 D=M A=A-1 A=M D=A-D  ('@ is-true) ("0;" operator)
	 @SP  A=M-1 M=0 ('@ end) ("0;JMP") ((make-label is-true)) @SP A=M-1 M=-1 ((make-label end)))))

(defun c_label (symbol)
  (out (( make-label *function-name* '$ symbol) )))
(defun c_goto (symbol)
  (out ('@ *function-name* '$ symbol) ("0;JMP")))
(defun c_if-goto (symbol)
  (out @SP AM=M-1 D=M ('@ *function-name* '$ symbol) ("D;JNE")))

(defun c_function (fname nlocal)
  (setf *function-name* fname)
  (let ((loopl (unique-label))
	(end (unique-label))) 
    (out ("(" *function-name* ")") ('@ nlocal) D=A ((make-label loopl))  ('@ end) D=D-1 ("D;JLT")
	 @SP A=M M=0 @SP M=M+1 ('@ loopl) ("0;JMP") ((make-label end)))))

(defun c_call (fname narg)
  (let ((return-address (unique-label)))
    (out ('@ return-address)  D=A @SP A=M M=D @SP M=M+1)
    (mapcar #'push-block registers)
    (out D=M ('@ narg) D=D-A @5 D=D-A @ARG M=D @SP D=M @LCL M=D ('@ fname) ("0;JMP")
	 ((make-label return-address)))))
(defun c_return ()
  (out @SP A=M-1 D=M @R13 M=D @ARG D=M @SP M=D @LCL D=M @R14 AM=M-1 D=M @THAT M=D
       @R14 AM=M-1 D=M @THIS M=D @R14 AM=M-1 D=M @ARG M=D @R14 AM=M-1 D=M @LCL M=D
       @R13 D=M @SP A=M M=D @SP M=M+1 @R14 A=M-1 A=M ("0;JUMP")))

(defun push-block (register)
  (out ('@ register) D=M @SP A=M M=D @SP M=M+1))
(defun unique-label ()
  (concatenate 'string "L" (write-to-string (incf *label-number*))))

;(process-batch "StaticTest.vm" "StaticTest")





