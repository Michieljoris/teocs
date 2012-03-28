(set 'program-structure ( class "class" className "{" classVarDec * subroutineDec* "}" 'e
				classVarDec ("static"  "field" ) type varName ("," varName) * ";" 'e
				type "int"  "char"  "boolean"  className 'e
				subroutineDec ("constructor"  "function"  "method") ("void"  type) subroutineName "(" parameterList ")" subroutineBody 'e
				parameterList ( (type varName) ("," type varName) *) ? 'e 
				subroutineBody "{" varDec * statements "}" 'e
				varDec "var" type varName ("," varName) * ";" 'e
				className identifier 'e
				subroutineName identifier 'e
				varName identifier 'e)) 


(defun make-identifiers (collection)
  )

