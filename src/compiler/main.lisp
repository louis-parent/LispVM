(defun compile_lisp (expressions)
	(if (null expressions)
		'()
		(append (compile_lisp_expression (car expressions)) (compile_lisp (cdr expressions)))
	)
)

(defun compile_lisp_expression (expression)
	(if (atom expression)
		
		; Cas d'un littéral ou d'un symbole
		(if (numberp expression)
			(compile_lit_expression expression)
			(cons expression '()) ; TODO : Symbole de variable à interpréter => ajout de l'environement
		)
		
		; Cas d'une expression à évaluer
		(let (
				(operator (car expression))
				(args (cdr expression))
			)
			(cond 
				((eq operator '+)
					(compile_add_expression args)
				)
				; TODO Switch sur tout les opérateurs de base
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
				(append (compile_lisp_expression left) '((PUSH R0))) ; Sauvegarde de l'opérante de gauche dans la pile
				(append (compile_lisp_expression right) '((PUSH R0))); Sauvegarde de l'opérante de droite dans la pile
			)
			'(
				(POP R0) ; Récupération de l'opérante de droite
				(POP R1) ; Récupération de l'opérante de gauche
				(ADD R1 R0) ; Calcul du résultat et stockage dans R0
			)
		)
	)
)

(defun compile_lit_expression (value)
	(cons (list 'MOVE (list ':CONST value) 'R0) NIL) ; Stockage de la valeur dans R0
)

(compile_lisp '((+ 1 1)))
