(define-condition on-var-never-initialized (error)
	((message :initarg :message :reader message))
)

(defun compile_lisp (expressions)
	(setf (get '__LISP_COMPILER__ '__GLOBALS__) '())
	(setf (get '__LISP_COMPILER__ '__LABELS__) '())
	(setf (get '__LISP_COMPILER__ '__FRAME_OFFSET_) '())
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
		(progn
			(if (or (or (or (numberp expression) (eq 'NIL expression)) (eq 'T expression)) (stringp expression))
				(compile_lit_expression expression)
				(compile_var_expression expression environment)
			)
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
				((eq operator 'cond)
					(compile_cond_expression args environment)
				)
				((eq operator 'if)
					(compile_if_expression args environment)
				)
				((eq operator '=)
					(compile_equal_expression args environment)
				)
				((eq operator '/=)
					(compile_not_equal_expression args environment)
				)
				((eq operator '>)
					(compile_greater_expression args environment)
				)
				((eq operator '>=)
					(compile_greater_equal_expression args environment)
				)
				((eq operator '<)
					(compile_lower_expression args environment)
				)
				((eq operator '<=)
					(compile_lower_equal_expression args environment)
				)
				((eq operator 'null)
					(compile_null_expression args environment)
				)
				((eq operator 'not)
					(compile_not_expression args environment)
				)
				((eq operator 'let)
				  (compile_let_expression args environment)
				)
				((eq operator 'progn)
				  (compile_progn_expression args environment)
				)
				((eq operator 'setf)
					(compile_setf_expression args environment)
				)
				((eq operator 'QUOTE)
					(compile_quote_value args environment)
				)
				((= 1 1); Cas par default
					(compile_call_function operator args environment)
				)
			)
		)
	)
)

(defun compile_setf_expression (args environment)
	(append 
		(list 
			(list 'PUSH (list ':CONST (replace_var_by_environment_offset (car args) environment)))
			(append 
				(compile_lisp_expression (cadr args) environment)
				(append 
					(list (list 'PUSH 'R0))
					(append 
						(list (list 'PUSH (list ':CONST 2)))
						(list (list 'JSR 'setf) '(POP R1) '(POP R1) '(POP R1))
					)
				)
			)
		)
	)
)

(defun replace_var_by_environment_offset (instruction environment)
	(if (null instruction)
		'()
		(let ((environment_offset (get_offset_in_environment (car instruction) environment)))
			(if (null environment_offset)
				(cons (car instruction) (replace_var_by_environment_offset (cdr instruction) environment))
				(cons (list ':VAR environment_offset) (replace_var_by_environment_offset (cdr instruction) environment))
			)
		)
	)
)

(defun get_offset_in_environment (var environment)
	(if (null environment)
		NIL
		(let (
				(current_name (caar environment))
				(current_offset (cadar environment))
			)
			(if (eq var current_name)
				current_offset
				(get_offset_in_environment var (cdr environment))
			)
		)
	)
)

(defun compile_quote_value (args environment)
	(cons (list 'MOVE (list ':CONST (car args)) 'R0) NIL)
)

(defun compile_call_function (operator next environment)
	(append 
		(if (is_known_function operator (get '__LISP_COMPILER__ '__GLOBALS__))
			(compile_known_function operator next environment)
			(compile_lisp_fonction operator next environment)
		)
		(create_pop (+ (size_array next) 1))
	)
)

(defun create_pop (n)
	(if (= 0 n)
		'()
		(append (create_pop (- n 1)) (list '(POP R1)))
	)
)

(defun compile_lisp_fonction (func next environment)
	(let 
		(
			(nb_parameter (size_array next))
		)
		
		(append (compile_args_call_function_lisp next environment)
		(append (list (list 'PUSH (list ':CONST nb_parameter)))
		(append (list (list 'JSR func)))))
	)
)

(defun compile_known_function (func args environment)
	(let 
		(
			(nb_parameter (size_array args))
			
		)
		
		(append (compile_args_call_function args environment)
		(append (list (list 'PUSH (list ':CONST nb_parameter)))
		(append (list (list 'JSR func)))))
	)
)

(defun compile_args_call_function (args environment)
	(if (null args)
		'()
		(progn
			(append (compile_lisp_expressions args environment)
			(append (list (list 'PUSH 'R0))
			(compile_args_call_function (cdr args) environment)))
		)
	)
)

(defun compile_args_call_function_lisp (args environment)
	(if (null args)
		'()
		(progn
			(append (compile_lisp_expression (car args) environment)
			(append (list (list 'PUSH 'R0))
			(compile_args_call_function_lisp (cdr args) environment)))
		)
	)
)

(defun compile_lit_expression (value)
	(cons (list 'MOVE (list ':CONST value) 'R0) NIL) ; Stockage de la valeur dans R0
)

(defun compile_var_expression (name environment)
	(if (null environment)
		(error 'on-var-never-initialized :message (format t "~a~% doesn't exist" name)) ; La variable n'a pas été trouvé dans l'environment
		(let (
				(current (car environment))
			)
			(if (eq (car current) name)
				(list
					'(MOVE FP R1) ; Récupération de l'adresse du frame pointer
					(list 'ADD (list ':CONST (cadr current)) 'R1) ; Calcul de l'adresse de la variable selon l'offset
					'(LOAD R1 R0) ; Stockage de la valeur dans R0
				)
				(compile_var_expression name (cdr environment)) ; Sinon on cherche dans le reste de l'environment
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
				(append (compile_lisp_expression left environment) (list (create_push_instruction 'R0))) ; Sauvegarde de l'opérante de gauche dans la pile
				(append (compile_lisp_expression right environment) (list (create_push_instruction 'R0))); Sauvegarde de l'opérante de droite dans la pile
			)
			(list
				(create_pop_instruction 'R0) ; Récupération de l'opérante de droite
				(create_pop_instruction 'R1) ; Récupération de l'opérante de gauche
				'(ADD R1 R0) ; Calcul du résultat et stockage dans R0
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
				(append (compile_lisp_expression left environment) (list (create_push_instruction 'R0))) ; Sauvegarde de l'opérante de gauche dans la pile
				(append (compile_lisp_expression right environment) (list (create_push_instruction 'R0))); Sauvegarde de l'opérante de droite dans la pile
			)
			(list
				(create_pop_instruction 'R0) ; Récupération de l'opérante de droite
				(create_pop_instruction 'R1) ; Récupération de l'opérante de gauche
				'(SUB R1 R0) ; Calcul du résultat et stockage dans R0
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
				(append (compile_lisp_expression left environment) (list (create_push_instruction 'R0))) ; Sauvegarde de l'opérante de gauche dans la pile
				(append (compile_lisp_expression right environment) (list (create_push_instruction 'R0))); Sauvegarde de l'opérante de droite dans la pile
			)
			(list
				(create_pop_instruction 'R0) ; Récupération de l'opérante de droite
				(create_pop_instruction 'R1) ; Récupération de l'opérante de gauche
				'(MUL R1 R0) ; Calcul du résultat et stockage dans R0
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
				(append (compile_lisp_expression left environment) (list (create_push_instruction 'R0))) ; Sauvegarde de l'opérante de gauche dans la pile
				(append (compile_lisp_expression right environment) (list (create_push_instruction 'R0))); Sauvegarde de l'opérante de droite dans la pile
			)
			(list
				(create_pop_instruction 'R0) ; Récupération de l'opérante de droite
				(create_pop_instruction 'R1) ; Récupération de l'opérante de gauche
				'(DIV R1 R0) ; Calcul du résultat et stockage dans R0
			)
		)
	)
)

(defun compile_defun_expression (args environment)
	(let (
			(name (car args))
			(params (cadr args))
			(body (cddr args))
			(label_skip (create_label "skip_function"))
		)
		(add_function_to_globals name (create_function_params_offset_list params))
		(push_new_frame_context)
		(let (
				(bytecode (append 
							(list
								(list 'JMP label_skip)
								(list 'LABEL name)
							)
							(append
								(compile_lisp_expressions body (append environment (create_function_params_offset_list params)))
								(list 
									(list 'RTN)
									(list 'LABEL label_skip)
								)
							)
						)				
				)
			)
			(pop_new_frame_context)
			bytecode
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

(defun compile_cond_expression (next environment)
	(let 
		(
			(body (car (comparison_cond_to_if next environment)))
		)
	
		(compile_lisp_expression body environment)
	)
)

(defun comparison_cond_to_if (args environment)
	(let
		(
			(next_conds (cdr args))
			(first_cond (caar args))
			(first_do (cadar args))
		)
		
		(if (null args)
			(list (list));NIL avec le append
			(list
				(append
					(list
						'if
						first_cond
						first_do
					)
					(comparison_cond_to_if next_conds environment)
				)
			)
		)
		
	)
	
)

(defun compile_if_expression (next environment)
	(let 
		(
			(arg (compile_lisp_expression (car next) environment))
			(body_true (compile_lisp_expression (cadr next) environment))
			(body_false (compile_lisp_expression (caddr next) environment))
			(label_true (create_label "if_true"))
			(label_end (create_label "if_end"))
		)
		
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

(defun compile_equal_expression (args environment)
	(let 
		(
			(arg1 (compile_lisp_expression (car args) environment))
			(arg2 (compile_lisp_expression (cadr args) environment))
			(label_true (create_label "cmp_equal_true"))
			(label_end (create_label "cmp_equal_end"))
		)
		
		(append
			(append arg1
			(append (list (create_push_instruction 'R0))
			(append arg2)
			(append (list (create_pop_instruction 'R1))
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

(defun compile_not_equal_expression (args environment)
	(let 
		(
			(arg1 (compile_lisp_expression (car args) environment))
			(arg2 (compile_lisp_expression (cadr args) environment))
			(label_true (create_label "cmp_not_equal_true"))
			(label_end (create_label "cmp_not_equal_end"))
		)
		
		(append
			(append arg1
			(append (list (create_push_instruction 'R0))
			(append arg2)
			(append (list (create_pop_instruction 'R1))
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

(defun compile_greater_expression (args environment)
	(let 
		(
			(arg1 (compile_lisp_expression (car args) environment))
			(arg2 (compile_lisp_expression (cadr args) environment))
			(label_true (create_label "cmp_greater_true"))
			(label_end (create_label "cmp_greater_end"))
		)
		
		(append
			(append arg1
			(append (list (create_push_instruction 'R0))
			(append arg2)
			(append (list (create_pop_instruction 'R1))
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

(defun compile_greater_equal_expression (args environment)
	(let 
		(
			(arg1 (compile_lisp_expression (car args) environment))
			(arg2 (compile_lisp_expression (cadr args) environment))
			(label_true (create_label "cmp_greater_equal_true"))
			(label_end (create_label "cmp_greater_equal_end"))
		)
		
		(append
			(append arg1
			(append (list (create_push_instruction 'R0))
			(append arg2)
			(append (list (create_pop_instruction 'R1))
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

(defun compile_lower_expression (args environment)
	(let 
		(
			(arg1 (compile_lisp_expression (car args) environment))
			(arg2 (compile_lisp_expression (cadr args) environment))
			(label_true (create_label "cmp_lower_true"))
			(label_end (create_label "cmp_lower_end"))
		)
		
		(append
			(append arg1
			(append (list (create_push_instruction 'R0))
			(append arg2)
			(append (list (create_pop_instruction 'R1))
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

(defun compile_lower_equal_expression (args environment)
	(let 
		(
			(arg1 (compile_lisp_expression (car args) environment))
			(arg2 (compile_lisp_expression (cadr args) environment))
			(label_true (create_label "cmp_lower_equal_true"))
			(label_end (create_label "cmp_lower_equal_end"))
		)
		
		(append
			(append arg1
			(append (list (create_push_instruction 'R0))
			(append arg2)
			(append (list (create_pop_instruction 'R1))
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

(defun compile_null_expression (args environment)
	(let 
		(
			(arg (compile_lisp_expression (car args) environment))
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

(defun compile_not_expression (args environment)
	(let 
		(
			(arg (compile_lisp_expression (car args) environment))
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

(defun compile_let_expression (args environment)
	(let (
			(vars (car args))
			(expressions (cdr args))
			(local_environment (create_environment_for_locals (car args)))
		)
		(append
			(create_push_for_locals vars environment)
			(append
				(compile_lisp_expressions expressions (append environment local_environment))	
				(create_pop_for_locals vars)
			)
		)
	)
)

(defun create_push_for_locals (vars environment)
	(if (null vars)
		NIL
		(append
			(compile_lisp_expression (cadar vars) environment)
			(append 
				(list '(PUSH R0))
				(create_push_for_locals (cdr vars) environment)
			)
		)
	)
)

(defun create_pop_for_locals (vars)
	(if (null vars)
		NIL
		(append 
			(list (create_pop_instruction 'R2))
			(create_pop_for_locals (cdr vars))
		)
	)
)

(defun create_environment_for_locals (vars)
	(if (null vars)
		NIL
		(cons 
			(list
				(caar vars) 
				(progn 
					(increment_frame_offset)
					(car (last (get '__LISP_COMPILER__ '__FRAME_OFFSET_)))
				)
			)
			(create_environment_for_locals (cdr vars))
		)
	)
)

(defun push_new_frame_context ()
	(setf (get '__LISP_COMPILER__ '__FRAME_OFFSET_) (append (get '__LISP_COMPILER__ '__FRAME_OFFSET_) '(3)))
)

(defun pop_new_frame_context ()
	(setf (get '__LISP_COMPILER__ '__FRAME_OFFSET_) (without_last (get '__LISP_COMPILER__ '__FRAME_OFFSET_)))
)

(defun increment_frame_offset ()
	(setf (get '__LISP_COMPILER__ '__FRAME_OFFSET_) (increment_last (get '__LISP_COMPILER__ '__FRAME_OFFSET_)))
)

(defun decrement_frame_offset ()
	(setf (get '__LISP_COMPILER__ '__FRAME_OFFSET_) (decrement_last (get '__LISP_COMPILER__ '__FRAME_OFFSET_)))
)

(defun increment_last (l)
	(if (null l)
		NIL
		(if (null (cdr l))
			(cons (+ (car l) 1) NIL)
			(cons (car l) (increment_last (cdr l)))
		)
	)
)

(defun decrement_last (l)
	(if (null l)
		NIL
		(if (null (cdr l))
			(cons (- (car l) 1) NIL)
			(cons (car l) (decrement_last (cdr l)))
		)
	)
)

(defun without_last (l)
	(reverse (cdr (reverse l)))
)

(defun create_push_instruction (register)
	(increment_frame_offset)
	(list 'PUSH register)
)

(defun create_pop_instruction (register)
	(decrement_frame_offset)
	(list 'POP register)
)

(defun compile_progn_expression (args environment)
	(compile_lisp_expressions args environment)
)

(defun size_array (array)
	(if (null array)
		0
		(+ (size_array (cdr array)) 1)
	)
)

(defun is_known_function (operator array)
	(if (null array)
		NIL
		(if (eq operator (nth 1 (car array)))
			T
			(is_known_function operator (cdr array))
		)
	)
)

(compile_lisp '(
	(write "Hello World !")
))
