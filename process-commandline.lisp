
(in-package :com.michieljoris.eocs)

;sbcl --noinform --eval '(load "jc.fasl")' --eval '(quit)'

(setf (readtable-case *readtable*) :invert)
(defun main ()
  (if (boundp '*posix-argv*) (format t "~a~%" "yes") (print "no")))
;(let (( path (file-exists-p arg1))) (if (directory-pathname-p path) (let files (list))))
;;; this function compiles and loads all files in the current dir or a specified dir.
;;; it only compiles files out of date, it does load all of them.
;;; it optionally make then a core image (see bash script for syntax)
;;; the core image will be a command line util
; no error checking, but done functionally.
(defun get-args (args)
  (labels ((p-a (a) 
	     (cond
	       ((null a) ())
	       ((equal (car a) "-from")
		(cons (cons 'from (cadr a)) (p-a (cddr a))))
	       ((equal (car a) "-to")
		(cons (cons 'to (cadr a)) (p-a (cddr a))))
	       (t
		(cons (cons 'file-or-dir (car a)) (p-a (cdr a)))))))
    (p-a args)))
    
(defun compile-down (args)
  (let* ((file-list ())
	 (file-or-dir (cdr (assoc 'file-or-dir args)))
	 (from (cdr (assoc 'from args)))
	 (to (cdr (assoc 'to args)))
	 (pathname (file-exists-p file-or-dir)))
    (if pathname
	(case (directory-pathname-p pathname)
	  ((nil)
	   (setf file-list (list pathname))
	   (setf from (pathname-type file-or-dir)))
	  (otherwise
	   (setf file-list ( list-directory pathname))
	   (setf pathname (pathname-as-file pathname))
	   )))
    (setf file-list (filter file-list from))
    (if (equal from 'jack)
	(setf file-list (compile-jack file-list))
	(if (or ( equal to "asm") (equal to "hack"))
	    (setf from 'vm))) 
    (if (equal from 'vm)
	(compile-vm file-list
		    (setf pathname (make-pathname :type "asm" :defaults pathname)))
	(if (equal to "hack") (setf from "asm")))
    (if (equal from 'hack) (compile-asm pathname))
    (print from) (print to) (print file-list)))

(defun filter (list type)
  (remove-if (lambda (x)
		   (or ( not (equal (pathname-type x) type))
		       (directory-pathname-p x)))
		 list))


;( print (get-args (list "-from"  "-to" "bla" "filename" )))
;(print ( translate (get-args (list "-from" "vm" "-to" "asm" "max/Max.asm"))))
;(print ( file-exists-p "maxd"))

;(print  (filter ( list-directory "") "asm"))

;(print (make-pathname :type "bla" :defaults ( pathname-as-file ( file-exists-p "max"))))



