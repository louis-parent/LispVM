(define-condition on-var-never-initialized (error)
   ((message :initarg :message :reader message))	
)

(defun compile_lisp (expressions)
	(setf (get '__LISP_COMPILER__ '__GLOBALS__) '())
	(setf (get '__LISP_COMPILER__ '__LABELS__) '())
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
		(if (or (or (numberp expression) (eq 'NIL expression)) (eq 'T expression))
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
				((eq operator 'if)
					(compile_if_expression args environment)
				)
				((eql operator '=)
                    (compile_equal_expression args environment)
                )
                ((eql operator '/=)
                    (compile_not_equal_expression args environment)
                )
                ((eql operator '>)
                    (compile_greater_expression args environment)
                )
                ((eql operator '>=)
                    (compile_greater_equal_expression args environment)
                )
                ((eql operator '<)
                    (compile_lower_expression args environment)
                )
                ((eql operator '<=)
                    (compile_lower_equal_expression args environment)
                )
                ((eql operator 'null)
                    (compile_null_expression args environment)
                )
                ((eql operator 'not)
                    (compile_not_expression args environment)
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

(defun compile_if_expression (next environement)
    (let 
        (
            (arg (compile_lisp_expression (car next) environement))
            (body_true (compile_lisp_expression (cadr next) environement))
            (body_false (compile_lisp_expression (caddr next) environement))
            (label_true (create_label "if_true"))
            (label_end (create_label "if_end"))
        )
        
        ;(write-line (nth 1 (car next)))
        ;(write-line (caddr next))
        
        (append
            (append arg
            (append (list (list 'TEST 'R0))
            (append (list (list 'JTRUE label_true))
            (append body_false
            (append (list (list 'JMP label_end))
            (append (list (list 'LABEL label_true))
            (append body_true
            (append (list (list 'JMP label_end))
            (append (list (list 'LABEL label_end)))))))))))
        )
    )
)

(defun compile_equal_expression (args environement)
    (let 
        (
            (arg1 (compile_lisp_expression (car args) environement))
            (arg2 (compile_lisp_expression (cadr args) environement))
            (label_true (create_label "cmp_equal_true"))
            (label_end (create_label "cmp_equal_end"))
        )
        
        (append
            (append arg1
            (append (list (list 'PUSH 'R0))
            (append arg2)
            (append (list (list 'POP 'R1))
            (append (list (list 'CMP 'R1 'R0))
            (append (list (list 'JEQ label_true))
            (append (list (list 'MOVE '(:CONST NIL) 'R0))
            (append (list (list 'JMP label_end))
            (append (list (list 'LABEL label_true))
            (append (list (list 'MOVE '(:CONST T) 'R0))
            (append (list (list 'LABEL label_end))))))))))))
        )
    )
)

(defun compile_not_equal_expression (args environement)
    (let 
        (
            (arg1 (compile_lisp_expression (car args) environement))
            (arg2 (compile_lisp_expression (cadr args) environement))
            (label_true (create_label "cmp_not_equal_true"))
            (label_end (create_label "cmp_not_equal_end"))
        )
        
        (append
            (append arg1
            (append (list (list 'PUSH 'R0))
            (append arg2)
            (append (list (list 'POP 'R1))
            (append (list (list 'CMP 'R1 'R0))
            (append (list (list 'JNE label_true))
            (append (list (list 'MOVE '(:CONST NIL) 'R0))
            (append (list (list 'JMP label_end))
            (append (list (list 'LABEL label_true))
            (append (list (list 'MOVE '(:CONST T) 'R0))
            (append (list (list 'LABEL label_end))))))))))))
        )
    )
)

(defun compile_greater_expression (args environement)
    (let 
        (
            (arg1 (compile_lisp_expression (car args) environement))
            (arg2 (compile_lisp_expression (cadr args) environement))
            (label_true (create_label "cmp_greater_true"))
            (label_end (create_label "cmp_greater_end"))
        )
        
        (append
            (append arg1
            (append (list (list 'PUSH 'R0))
            (append arg2)
            (append (list (list 'POP 'R1))
            (append (list (list 'CMP 'R1 'R0))
            (append (list (list 'JGT label_true))
            (append (list (list 'MOVE '(:CONST NIL) 'R0))
            (append (list (list 'JMP label_end))
            (append (list (list 'LABEL label_true))
            (append (list (list 'MOVE '(:CONST T) 'R0))
            (append (list (list 'LABEL label_end))))))))))))
        )
    )
)

(defun compile_greater_equal_expression (args environement)
    (let 
        (
            (arg1 (compile_lisp_expression (car args) environement))
            (arg2 (compile_lisp_expression (cadr args) environement))
            (label_true (create_label "cmp_greater_equal_true"))
            (label_end (create_label "cmp_greater_equal_end"))
        )
        
        (append
            (append arg1
            (append (list (list 'PUSH 'R0))
            (append arg2)
            (append (list (list 'POP 'R1))
            (append (list (list 'CMP 'R1 'R0))
            (append (list (list 'JGT label_true))
            (append (list (list 'MOVE '(:CONST NIL) 'R0))
            (append (list (list 'JMP label_end))
            (append (list (list 'LABEL label_true))
            (append (list (list 'MOVE '(:CONST T) 'R0))
            (append (list (list 'LABEL label_end))))))))))))
        )
    )
)

(defun compile_lower_expression (args environement)
    (let 
        (
            (arg1 (compile_lisp_expression (car args) environement))
            (arg2 (compile_lisp_expression (cadr args) environement))
            (label_true (create_label "cmp_lower_true"))
            (label_end (create_label "cmp_lower_end"))
        )
        
        (append
            (append arg1
            (append (list (list 'PUSH 'R0))
            (append arg2)
            (append (list (list 'POP 'R1))
            (append (list (list 'CMP 'R1 'R0))
            (append (list (list 'JLT label_true))
            (append (list (list 'MOVE '(:CONST NIL) 'R0))
            (append (list (list 'JMP label_end))
            (append (list (list 'LABEL label_true))
            (append (list (list 'MOVE '(:CONST T) 'R0))
            (append (list (list 'LABEL label_end))))))))))))
        )
    )
)

(defun compile_lower_equal_expression (args environement)
    (let 
        (
            (arg1 (compile_lisp_expression (car args) environement))
            (arg2 (compile_lisp_expression (cadr args) environement))
            (label_true (create_label "cmp_lower_equal_true"))
            (label_end (create_label "cmp_lower_equal_end"))
        )
        
        (append
            (append arg1
            (append (list (list 'PUSH 'R0))
            (append arg2)
            (append (list (list 'POP 'R1))
            (append (list (list 'CMP 'R1 'R0))
            (append (list (list 'JLE label_true))
            (append (list (list 'MOVE '(:CONST NIL) 'R0))
            (append (list (list 'JMP label_end))
            (append (list (list 'LABEL label_true))
            (append (list (list 'MOVE '(:CONST T) 'R0))
            (append (list (list 'LABEL label_end))))))))))))
        )
    )
)

(defun compile_null_expression (args environement)
    (let 
        (
            (arg (compile_lisp_expression (car args) environement))
            (label_true (create_label "cmp_null_true"))
            (label_end (create_label "cmp_null_end"))
        )
        
        (append
            (append arg
            (append (list (list 'TEST 'R0))
            (append (list (list 'JNIL label_true))
            (append (list (list 'MOVE '(:CONST NIL) 'R0))
            (append (list (list 'JMP label_end))
            (append (list (list 'LABEL label_true))
            (append (list (list 'MOVE '(:CONST T) 'R0))
            (append (list (list 'LABEL label_end))))))))))
        )
    )
)

(defun compile_not_expression (args environement)
    (let 
        (
            (arg (compile_lisp_expression (car args) environement))
            (label_true (create_label "cmp_null_true"))
            (label_end (create_label "cmp_null_end"))
        )
        
        (append
            (append arg
            (append (list (list 'TEST 'R0 ))
            (append (list (list 'JTRUE label_true))
            (append (list (list 'MOVE '(:CONST T) 'R0))
            (append (list (list 'JMP label_end))
            (append (list (list 'LABEL label_true))
            (append (list (list 'MOVE '(:CONST NIL) 'R0))
            (append (list (list 'LABEL label_end))))))))))
        )
    )
)

(defun create_label (context)
    (make_label context 0)
)

(defun make_label (context value)
    (if (exist (read-from-string (concatenate 'string context (write-to-string value))) (get '__LISP_COMPILER__ '__LABELS__))
        (make_label context (+ value 1))
        (progn
            (setf (get '__LISP_COMPILER__ '__LABELS__) (append (get '__LISP_COMPILER__ '__LABELS__) (list (read-from-string (concatenate 'string context (write-to-string value))))))
            (read-from-string (concatenate 'string context (write-to-string value)))
        )
        
    )
)

(defun exist (value array)
    (if (null array)
        NIL
        (if (atom (car array))
            (if (eq value (car array))
                T
                (exist value (cdr array))
            )
            NIL
        )
    )
)

;(write (compile_lisp '(
;    (not (+ 4 5))
;)))

(write (compile_lisp '(
    (if (> (+ 11 2) (- 6 1))
        (if  (not (null (/= 3 4)))
            1
            0
        )
        (+ 2 1)
    )
)))
