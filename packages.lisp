(in-package :common-lisp)
(defpackage :com.gigamonkeys.pathnames
  (:use :common-lisp)
  (:nicknames :pathnames)
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))
(defpackage :com.michieljoris.eocs.language-definition
  (:nicknames :language-definition)
  (:use :cl)
  (:export :load-language-definition-file :get-toplevel-construct-definition
   :get-construct-definition :get-construct-definition-readable))
(defpackage :com.michieljoris.eocs.tokenizer
  (:nicknames :tokenizer)
  (:use :cl)
  (:export :load-tokens
	   :get-token
	   :set-mark
	   :go-back-to-last-set-mark
	   :discard-last-set-mark
	   :alpha_
	   :sign
	   :string
	   :number))
(defpackage :com.michieljoris.eocs.tree-walker
  (:use :cl :tokenizer :language-definition)
  (:nicknames :tree-walker)
  (:export :walk-tree))
(defpackage :com.michieljoris.eocs.ast-builder
  (:use :cl :tree-walker)
  (:nicknames :ast-builder)
  (:export :get-sexpr-string))
(defpackage :com.michieljoris.eocs.jack-compiler
  (:use :cl)
  (:nicknames :jack-compiler)
  (:export :compile-jack))
(defpackage :com.michieljoris.hack-assembler
  (:use :cl :pathnames :pregexp)
  (:nicknames :hack-assembler)
  (:export :compile-asm))
(defpackage :com.michieljoris.eocs.vm-compiler
  (:use :cl)
  (:nicknames :vm-compiler)
  (:export :compile-vm))
(defpackage :com.michieljoris.eocs.vm-writer
  (:use :cl :ast-builder)
  (:nicknames :vm-writer)
  (:export :compile-vm))
(defpackage :com.michieljoris.eocs
 (:use :cl pathnames :jack-compiler :vm-compiler :hack-assembler)
  (:nicknames :eocs)
  (:export)
)

