(setq environement_global '())
(setq labels '())

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
			(list (list expression)) ; TODO : Symbole de variable à interpréter => ajout de l'environement
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
				((eq operator '-)
					(compile_sub_expression args)
				)
				((eq operator '*)
					(compile_mul_expression args)
				)
				((eq operator '/)
					(compile_div_expression args)
				)
				((eq operator 'if)
					(compile_if_expression args)
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

(defun compile_sub_expression (args)
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
				(SUB R1 R0) ; Calcul du résultat et stockage dans R0
			)
		)
	)
)

(defun compile_mul_expression (args)
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
				(MUL R1 R0) ; Calcul du résultat et stockage dans R0
			)
		)
	)
)

(defun compile_div_expression (args)
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
				(DIV R1 R0) ; Calcul du résultat et stockage dans R0
			)
		)
	)
)

(defun compile_if_expression (next)
    (let 
        (
            (operator (caar next))
            (arg1 (compile_lisp (list (nth 1 (car next)))));(PUSH R0)
            (arg2 (compile_lisp (list (nth 2 (car next)))));(PUSH R0)
            (body_true (list (cadr next)))
            (body_false (list (caddr next)))
            (label_true (create_label "if_true"))
            (label_false (create_label "if_false"))
            (label_end (create_label "if_end"))
        )
        
        ;(write-line (nth 1 (car next)))
        ;(write-line arg2)
        
        (append
            (append (compile_comparison operator arg1 arg2 label_true)
            (append (list (list 'LABEL label_false))
            (append  (compile_lisp body_false)
            (append (list (list 'JMP label_end))
            (append (list (list 'LABEL label_true))
            (append  (compile_lisp body_true)
            (append (list (list 'JMP label_end))
            (list (list 'LABEL label_end)))))))))
        )
    )
)

(defun compile_comparison (operator arg1 arg2 return_point)
    (progn
        (append arg1 
        (append '((PUSH R0))
        (append arg2
        (append '((POP R1) (CMP R1 R0))
        (cond
            ((eql operator '=)
                (list (list 'JEQ return_point))
            )
            ((eql operator '>)
                (list (list 'JGT return_point))
            )
            ((eql operator '>=)
                (list (list 'JGE return_point))
            )
            ((eql operator '<)
                (list (list 'JLT return_point))
            )
            ((eql operator '<=)
                (list (list 'JLE return_point))
            )
            ((eql operator '/=)
                (list (list 'JNE return_point))
            )
            ;ajouter les condition si null
        )))))
    )
)

(defun create_label (context)
    (make_label context 0)
)

(defun make_label (context value)
    (if (exist (read-from-string  (concatenate 'string context (write-to-string value))) labels)
        (make_label context (+ value 1))
        (progn
            (push (read-from-string (concatenate 'string context (write-to-string value))) labels)
            (read-from-string (concatenate 'string context (write-to-string value)))
        )
        
    )
)

(defun exist (value array)
    (if (null array)
        NIL
        (if (atom (car array))
            (if (string= value (car array))
                T
                (exist value (cdr array))
            )
            NIL
        )
    )
)

(defun compile_lit_expression (value)
	(cons (list 'MOVE (list ':CONST value) 'R0) NIL) ; Stockage de la valeur dans R0
)

(write (compile_lisp '(
    (if (> (+ 11 2) (- 6 1))
        (if (/= 3 4)
            1
            0
        )
        (+ 2 1)
    )
)))
