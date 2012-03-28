(in-package :hack-assembler)
;(load "c:\\home\\mysrc\\lisp\\\eocs\\lib\\pregexp.wx64fsl")

;;see bottom of file to fill in file to compile. Or comment it out and say (process-file "name of file") in lisp repl

;;this is not a fast implementation, maybe the regex package is at fault, maybe me, 
;;It's basically one-pass, with extra step to fill
;;in the unresolved labels at the end. Maybe the data structures could be faster iso using lists
;;The method used is different than the one in the book. . Learnt regexps and lisp with it.
;;a limited repl is included for testing. Also helper methods for printing out stuff. Error checking included.
;;if sbcl is used this code is fastest. With cmucl also fast, seems a lot of garbage collection is going on. 
;;clisp is slow. Still not as fast as the supplied assembler.

(defparameter *args* (list "./pong/Pong.asm" ))

(defparameter *computations* (list '|0|     "0101010"
                                   '|1|     "0111111"
                                   '|-1|    "0111010"
                                   '|D|     "0001100"
				   '|A|     "0110000"
				   '|!D|    "0001101"
				   '|!A|    "0110001"
				   '|-D|    "0001111"
				   '|-A|    "0110011"
				   '|D+1|   "0011111"
				   '|A+1|   "0110111"
				   '|D-1|   "0001110"
				   '|A-1|   "0110010"
				   '|D+A|   "0000010"
				   '|D-A|   "0010011"
				   '|A-D|   "0000111"
				   '|D&A|   "0000000"
				   '|D\|A|   "0010101"
				   
				   '|M|     "1110000"
				   '|!M|    "1110001"
				   '|-M|    "1110011"
				   '|M+1|   "1110111"
				   '|M-1|   "1110010"
				   '|D+M|   "1000010"
				   '|D-M|   "1010011"
				   '|M-D|   "1000111"
				   '|D&M|   "1000000"
				   '|D\|M|  "1010101"
                                   ))

(defparameter *dest*               (list  'null "000"
					  'M    "001"
					  'D    "010"
					  'MD   "011"
					  'A    "100"
					  'AM   "101"
					  'AD   "110"
					  'AMD  "111"))
(defparameter *jump*  (list 'null   "000"
			    'JGT    "001"
			    'JEQ    "010"
			    'JGE    "011"
			    'JLT    "100"
			    'JNE    "101"
			    'JLE    "110"
			    'JMP    "111"))

(defparameter *predefined-labels* (list 'SP 0 'LCL 1 'ARG 2 'THIS 3 'THAT 4
					'SCREEN 16384 'KBD 24576
					'R0 0 'R1 1 'R2 2 'R3 3 'R4 4 'R5 5 'R6 6 'R7 7 'R8 8 'R9 9
					'R10 10 'R11 11 'R12 12 'R13 13 'R14 14 'R15 15
					))

(defparameter *labels* (make-hash-table :test 'equal))
(defparameter *unresolved-labels* ()) 
(defparameter *machine-instructions* (make-array 2 :fill-pointer 0 :adjustable t)) 
(defparameter *lines* (make-array 2 :fill-pointer 0 :adjustable t)) 
(defparameter *counter* 0)


(defmacro out (&rest args)
  `(format t "~a~%" (concatenate 'string ,@args)))

(defmacro add-label (label address)
  `(setf (gethash ,label *labels*) ,address))

(defmacro get-label (label)
  `(gethash ,label *labels*))

(defparameter *label-regex* "(?x:
                  (?<=^[[:space:]]*[(]{1})   ; lookback to line start then spaces* then opening parens 
                  [[:alpha:]_.$:]{1}[[:alnum:]_.$:]*   ;label
                  (?=[)]{1}[[:space:]]*(?://.*)?$)    ; lookahead to spaces* and //comments*
                  )")
(defparameter *a-number-regex* "(?x:
                           (?<=^[[:space:]]*[@]{1})   ; lookback to line start then spaces* then opening parens 
                            (?:                          
                             [[:digit:]]+          ;digit
                            )  
                  (?=[[:space:]]*(?://.*)?$)     ;pp lookahead to spaces* and //comments*
                           )")
(defparameter *a-label-regex* "(?x:
                           (?<=^[[:space:]]*[@]{1})   ; lookback to line start then spaces* then opening parens 
                            (?:                          
                              [[:alpha:]_.$:]{1}[[:alnum:]_.$:]* ;label  
                            )  
                  (?=[[:space:]]*(?://.*)?$)     ;pp lookahead to spaces* and //comments*
                           )")
(defparameter *c-regex* "(?x:
                             (?<=^[[:space:]]*)   ; lookback to line start then spaces*  
                             (?:([AMDnull]+(?=[=]{1}))=)?   ; optional destination
                             ([01DAM!+&|-]+){1}   ; instruction
                             (?:[;]{1}([JMPnullGTEQLN]{3,4}))?  ;optional jump
                  (?=[[:space:]]*(?://.*)?$)     ;pp lookahead to spaces* and //comments*

                         )")

(defparameter *empty-line-regex* "^[[:space:]]*(//.*)?$")

(defun init()
  (setf *labels* (make-hash-table :test 'equal))
  (setf *unresolved-labels* ()) 
  (setf *machine-instructions*  (make-array 2 :fill-pointer 0 :adjustable t))
  (setf *lines*  (make-array 2 :fill-pointer 0 :adjustable t))
  (setf *counter* 0))

(defun compile-asm (pathname) 
  ;;(init) not necessary for a command line run
  (out "Compiling " pathname)  
  (let ((in (open pathname :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
	 while line do 
	   ( process-line line)
	   )
      (close in)))
  (setf *counter* 0)
  (resolve-labels)
  (out "Finished")
  (if (second *args*)
      (progn 
	(format t "Unresolved labels and their line numbers: ~%~a~%" *unresolved-labels*)
	(print-symbols)
	;(map 'vector #'print-line *machine-instructions* *lines*)
	))
  (with-open-file (file-out 
		   (make-pathname :type "asm" :defaults pathname)
		   :direction :output
		   :if-exists :supersede)
    (map 'vector #'(lambda (x) (format file-out "~a~%" x)) *machine-instructions*)))

(defun resolve-labels () 
  (setf *unresolved-labels* (reverse *unresolved-labels*))
  ;;this reverse is necessary to match the output with the supplied asm's output
  (let ((unallocated 16))
    (loop for a in *unresolved-labels* do
	 (let ((place (second a))
	       (address  (getf *predefined-labels* (intern (first a)))))
	   (unless address  (setf address (gethash (first a) *labels*))) 
	   (unless address (progn
			     (setf address unallocated)
			     (add-label (first a) address)
			     (incf unallocated))
		   )
	   (setf (aref *machine-instructions* place) (convert-to-binary  address))))))

(defun print-line (instruction line)
  (format t "~a ~a ~a~%" *counter* instruction line)
  (incf *counter*))

(defun process-line (line) 
  (let ((result (eval-line line)))
    (if result (progn
                 (incf *counter*)
                 (vector-push-extend result *machine-instructions*   10)
                 (vector-push-extend line *lines*   10)
		 ))
 (if (eql (mod *counter* 1000) 1)
	(format t "Progress: ~a~%" *counter*))
    ))

(defun eval-line (line)
  (let* ((label-match (pregexp-match *label-regex* line))
	 (c-match (pregexp-match *c-regex* line))
	 (a-label-match (pregexp-match *a-label-regex* line))  
	 (a-number-match (pregexp-match *a-number-regex* line))
	 (empty-line-match (pregexp-match *empty-line-regex* line)))
    (cond  (label-match (process-label :name (car label-match)))
           (c-match     (process-c-instruction :dest (second c-match) 
					       :instruction (third c-match) :jump (fourth c-match)))
           (a-label-match     (process-a-label-instruction :label (car a-label-match)))
           (a-number-match     (process-a-number-instruction :number-string (car a-number-match)))
	   (empty-line-match nil)
           (t (concatenate 'string  "ERROR: " line) ))))

(defun process-label (&key name)
  (add-label name *counter*)
  nil)

(defun process-c-instruction (&key dest instruction jump)
  (let* ((d (if dest (getf *dest* (intern dest))  "000"))
	 (c (getf *computations* (intern instruction)))
	 (j (if jump ( getf *jump* (intern jump)) "000")))
    (if (and d c j) (concatenate 'string "111" c d j))))

(defun process-a-label-instruction (&key label)
  (push (list label *counter*) *unresolved-labels*)
  "0000000000000000")

(defun process-a-number-instruction (&key number-string)
  (let ((n (with-input-from-string (n number-string) (read n))))
    (convert-to-binary n)))

(defun convert-to-binary (n) 
    (if (> n 32767) (error "address-is-over-32767")
	(with-output-to-string (out) (format out "~16,'0B" n))))

(defun my-repl () 
  (init)
  (let ((line (read-line)) )
    (unless (equal line "q")
      (cond ((equal line "r") (init))
            ((equal line "s") (print-symbols))
            ((equal line "i") (print-machine-instructions)) 
            ((equal line "u") (print-unresolved-labels)) 
            (t (format t "~a~%" (process-line line)))) 
      (my-repl)))) 

(defun print-machine-instructions () 
  (format t "~a~%" *machine-instructions*)
  )

(defun print-unresolved-labels () 
  (format t "~a~%" *unresolved-labels*))

(defun print-symbols () 
  (format t "Location of labels: ~%")
  (maphash #'(lambda (key val) 
	       (format t "~a: ~a~%" key val)) *labels*))

;( compile-asm (car *args*))


