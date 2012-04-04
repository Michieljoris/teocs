(in-package :compilation-engine)

(setf (readtable-case *readtable*) :invert)

(defun push-value (segment offset)
 (concatenate 'string "push " segment " " offset))

(defun pop-value (segment offset)
 (concatenate 'string "pop " segment offset))

(defun write-arithmatic ()

  )

(defun write-label ()

  )

(defun write-goto ()

  )

(defun write-if ()

  )

(defun write-call ()

  )

(defun write-function ()

  )

(defun write-return ()

  )

