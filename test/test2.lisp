(setf (readtable-case *readtable*) :invert)

(set 'definitions 
     '(@class "class" className "{" classVarDec * subroutineDec * "}" @
	     classVarDec ("static"  "field" ) type varName ("," varName) * ";" @
	     type "int"  "char"  "boolean"  className @
	     subroutineDec ("constructor"  "function"  "method") ("void"  type) subroutineName "(" parameterList ")" subroutineBody @
	     parameterList ( (type varName) ("," type varName) *) ? @
	     subroutineBody "{" varDec * statements "}" @
	     varDec "var" type varName ("," varName) * ";" @
	     className identifier @
	     subroutineName identifier @
	     varName identifier @

       statements statement * @
       statement letStatement  ifStatement  whileStatement  doStatement  returnStatement @
       letStatement "let" varName ("[" expression "]") ? "=" expression ";" @
       ifStatement "if" "(" expression ")" "{" statements "}" ( "else" "{" statements "}" ) ? @
       whileStatement "while" "(" expression ")" "{" statements "}" @
       doStatement "do" subroutineCall ";" @
       ReturnStatement "return" expression ? ";" @

       expression term (op term) * @
       term integerConstant  stringConstant  keywordConstant  varName  varName "[" expression "]"  subroutineCall  "(" expression ")"  unaryOp term @
       subroutineCall subroutineName "(" expressionList ")"  ( className  varName) "." subroutineName "(" expressionList ")" @
       expressionList (expression ("," expression) * ) ? @
       op "+"  "-"  " *"  "/"  "&"  ""  "<"  ">"  "=" @
       unaryOp "-"  "~" @
       KeywordConstant "true"  "false"  "null"  "this" @))

(labels ((list-definitions (defs)
	   (cond ((null (car defs)) () )
		 (t (cons (labels ((read-def () 
				     (let ((p (pop defs))) 
				       (cond ((equal p '@) ())
					     (t (cons p (read-def)))))))
			    (read-def ))
			  (list-definitions defs)))))
	 (evaluate (def) 
	   (eval (list 'setq  (car def) (list 'quote (cdr def))))))
  (mapcar #'evaluate (list-definitions definitions))
    
