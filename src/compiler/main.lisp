(define-condition on-var-never-initialized (error)
   ((message :initarg :message :reader message))	
)

(defun compile_lisp (expressions)
	(setf (get '__LISP_COMPILER__ '__GLOBALS__) '())
	(compile_lisp_expressions expressions '())
)

(defun compile_lisp_expressions (expressions environment)
	(if (null expressions)
		'()
		(append (compile_lisp_expression (car expressions) environment) (compile_lisp_expressions (cdr expressions) environment))
	)
)

(defun compile_lisp_expression (expression environment)
	(if (atom expression)
		
		; Cas d'un littéral ou d'un symbole
		(if (numberp expression)
			(compile_lit_expression expression)
			(compile_var_expression expression environment)
		)
		
		; Cas d'une expression à évaluer
		(let (
				(operator (car expression))
				(args (cdr expression))
			)
			(cond 
				((eq operator '+)
					(compile_add_expression args environment)
				)
				((eq operator '-)
					(compile_sub_expression args environment)
				)
				((eq operator '*)
					(compile_mul_expression args environment)
				)
				((eq operator '/)
					(compile_div_expression args environment)
				)
				((eq operator 'defun)
				  (compile_defun_expression args environment)
				)
				; TODO Switch sur tout les opérateurs de base
			)
		)
	)
)

(defun compile_lit_expression (value)
	(cons (list 'MOVE (list ':CONST value) 'R0) NIL) ; Stockage de la valeur dans R0
)

(defun compile_var_expression (name environment)
	(if (null environment)
		(error 'on-var-never-initialized :message (format t "~a~% doesn't exist" name)) ; La variable n'a pas été trouvé dans l'environement
		(let (
				(current (car environment))
			)
			(if (eq (car current) name)
				(list
					'(MOVE FP R1) ; Récupération de l'adresse du frame pointer
					(list 'ADD (list ':CONST (cadr current)) 'R1) ; Calcul de l'adresse de la variable selon l'offset
					'(LOAD R1 R0) ; Stockage de la valeur dans R0
				)
				(compile_var_expression name (cdr environment)) ; Sinon on cherche dans le reste de l'environement
			)
		)
	)
)

(defun compile_add_expression (args environment)
	(let (
			(left (car args))
			(right (cadr args))
		)
		(append
			(append 
				(append (compile_lisp_expression left environment) '((PUSH R0))) ; Sauvegarde de l'opérante de gauche dans la pile
				(append (compile_lisp_expression right environment) '((PUSH R0))); Sauvegarde de l'opérante de droite dans la pile
			)
			'(
				(POP R0) ; Récupération de l'opérante de droite
				(POP R1) ; Récupération de l'opérante de gauche
				(ADD R1 R0) ; Calcul du résultat et stockage dans R0
			)
		)
	)
)

(defun compile_sub_expression (args environment)
	(let (
			(left (car args))
			(right (cadr args))
		)
		(append
			(append 
				(append (compile_lisp_expression left environment) '((PUSH R0))) ; Sauvegarde de l'opérante de gauche dans la pile
				(append (compile_lisp_expression right environment) '((PUSH R0))); Sauvegarde de l'opérante de droite dans la pile
			)
			'(
				(POP R0) ; Récupération de l'opérante de droite
				(POP R1) ; Récupération de l'opérante de gauche
				(SUB R1 R0) ; Calcul du résultat et stockage dans R0
			)
		)
	)
)

(defun compile_mul_expression (args environment)
	(let (
			(left (car args))
			(right (cadr args))
		)
		(append
			(append 
				(append (compile_lisp_expression left environment) '((PUSH R0))) ; Sauvegarde de l'opérante de gauche dans la pile
				(append (compile_lisp_expression right environment) '((PUSH R0))); Sauvegarde de l'opérante de droite dans la pile
			)
			'(
				(POP R0) ; Récupération de l'opérante de droite
				(POP R1) ; Récupération de l'opérante de gauche
				(MUL R1 R0) ; Calcul du résultat et stockage dans R0
			)
		)
	)
)

(defun compile_div_expression (args environment)
	(let (
			(left (car args))
			(right (cadr args))
		)
		(append
			(append 
				(append (compile_lisp_expression left environment) '((PUSH R0))) ; Sauvegarde de l'opérante de gauche dans la pile
				(append (compile_lisp_expression right environment) '((PUSH R0))); Sauvegarde de l'opérante de droite dans la pile
			)
			'(
				(POP R0) ; Récupération de l'opérante de droite
				(POP R1) ; Récupération de l'opérante de gauche
				(DIV R1 R0) ; Calcul du résultat et stockage dans R0
			)
		)
	)
)

(defun compile_defun_expression (args environment)
	(let (
			(name (car args))
			(params (cadr args))
			(body (cddr args))
		)
		(add_function_to_globals name (create_function_params_offset_list params))
		(append 
			(cons (list 'LABEL name) NIL)
			(append
				(compile_lisp_expressions body (append environment (create_function_params_offset_list params)))
				(cons (list 'RETURN) NIL)
			)	
		)
	)
)

(defun create_function_params_offset_list (params)
	(if (null params)
		NIL
		(let (
				(end (create_function_params_offset_list (cdr params)))
				(current (car params))
			)
			(cons 
				(list current (- (if (null end) 0 (cadar end)) 1))
				end
			)
		)
	)
)


(defun add_function_to_globals (name offsets)
	(setf (get '__LISP_COMPILER__ '__GLOBALS__) (append (get '__LISP_COMPILER__ '__GLOBALS__) 
		(cons (list ':FUNC name offsets) NIL)
	))
)

(compile_lisp '(
	(defun incr (x)
		(+ x 1)
	)
))
