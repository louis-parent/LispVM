(defun compile_lisp (expressions)
	(if (null expressions)
		'()
		(append (compile_lisp_expression (car expressions)) (compile_lisp (cdr expressions)))
	)
)

(defun compile_lisp_expression (expression)
	(if (atom expression)
		(if (numberp expression)
			(compile_lit_expression expression)
			(cons expression '())
		)
		(let (
				(operator (car expression))
				(args (cdr expression))
			)
			(cond 
				((eq operator '+)
					(compile_add_expression args)
				)
			)
		)
	)
)

(defun compile_add_expression (args)
	(let (
			(left (car args))
			(right (cadr args))
		)
		(append
			(append 
				(append (compile_lisp_expression left) '((PUSH R0)))
				(append (compile_lisp_expression right) '((PUSH R0)))
			)
			'(
				(POP R0)
				(POP R1)
				(ADD R1 R0)
			)
		)
	)
)

(defun compile_lit_expression (value)
	(cons (list 'MOVE (list ':CONST value) 'R0) NIL)
)

(compile_lisp '((+ 1 1)))
