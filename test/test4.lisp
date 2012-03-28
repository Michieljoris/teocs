(defparameter c '(@class Main {
		  static int v1 @sc }
		  ))

(defmacro @class ( &rest l)
  `(process-class ',(car l) ,(macroexpand-1 (cdr l))))

(defun check-symbol (s l)
  (if (not (equal (cadr l) s)) (progn (print "error") ( quit))) 
  )

(print ( macroexpand-1 c))

(eval c)
(defun process-class (identifier varlist routinelist)
  (print identifier)
  (print varlist)
  (print routinelist))
(quit)

(defmacro { (&rest r)
  `(  ,(if (equal (car r) 'static)  `(,@r)))) 
