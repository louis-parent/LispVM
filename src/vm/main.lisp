;compteur ordinale(PC), base pointer (BP), frame pointer (FP), stack pointer (SP), finCode (EC), begin code pointer(CP), finStack (ES), r0, r1, r2, stop, inf, equal, sup 

(defun vm_create (vm_name size_memory)
	(setf (get vm_name 'memory) (make-list size_memory))
	(setf (get vm_name 'PC) (floor (* size_memory 0.9)))
	(setf (get vm_name 'CP) (floor (* size_memory 0.9)))
	(setf (get vm_name 'BP) 0)
	(setf (get vm_name 'FP) 0)
	(setf (get vm_name 'SP) 0)
	(setf (get vm_name 'EC) (floor (* size_memory 0.9)))
	(setf (get vm_name 'ES) (- (get vm_name 'CP) 1))
	(setf (get vm_name 'R0) 0)
	(setf (get vm_name 'R1) 0)
	(setf (get vm_name 'R2) 0)
	(setf (get vm_name 'STOP) NIL); false
	(setf (get vm_name 'INF) NIL)
	(setf (get vm_name 'EQU) NIL)
	(setf (get vm_name 'SUP) NIL)
	(setf (get vm_name 'FNIL) NIL)
	(setf (get vm_name 'symbols) '())
)

(defun vm_load (vm_name instructions)
	(vm_load_code vm_name instructions)
	(setf 
		(get vm_name 'symbols) 
		(append 
			(get vm_name 'symbols)
			(vm_load_symbols vm_name (nthcdr (floor (get vm_name 'CP)) (get vm_name 'memory)) 0)
		)
	);création de la liste des symboles présents
)

(defun vm_load_symbols (vm_name instructions instruction_index)
	(if (null instructions )
		'()
		(if (eq (caar instructions) 'LABEL)
			(cons (cons (cadar instructions) (cons instruction_index '()) ) (vm_load_symbols vm_name (cdr instructions) (+ instruction_index 1)))
			(vm_load_symbols vm_name (cdr instructions) (+ instruction_index 1))
		)
	)
)

(defun vm_load_code (vm_name instructions)
	(if (null instructions)
		()
		(progn
			(setf (get vm_name 'memory) (array_set (get vm_name 'memory) (get vm_name 'EC) (car instructions)))
			(setf (get vm_name 'EC) (+ 1 (get vm_name 'EC)))
			(vm_load_code vm_name (cdr instructions))
		)
	)
)

(defun array_set (l n value)
	(setq index n)
	(setq current l)
	
	(loop
		(when (<= index 0) (return (setf current value)))
		(setq index (- index 1))
		(setq current (cdr current))
	)
	
	l
)

(defun map_get (l key)
	(if (null l)
		NIL
		(if (eq (caar l) key)
			(cadar l)
			(map_get (cdr l) key)
		)
	)
)

(defun vm_get_value (vm_name argument)
	(if (listp argument)
		(cadr argument)
		(get vm_name argument)
	)
)

(defun vm_get_address (vm_name argument)
	(if (listp argument)
		(get vm_name (cadr argument))
		argument
	)
)

(defun vm_run (vm_name)	
	(if (= (get vm_name 'PC) (get vm_name 'EC))
		(get vm_name 'R0) ;fin de la vm
		(let ((current_instruction (get_current_instruction vm_name)))
			(setf (get vm_name 'PC) (+ (get vm_name 'PC) 1))
			(vm_run_instruction vm_name current_instruction)
			(vm_run vm_name)
		)
	)
)

(defun get_current_instruction (vm_name)
	(nth (get vm_name 'PC) (get vm_name 'memory))
)

(defun vm_run_instruction (vm_name current_instruction)
	(let ((operator (car  current_instruction)) (arguments (cdr current_instruction)) )		
		(cond
			((eql operator 'LOAD)
				(vm_run_load vm_name arguments)
			) 
			((eql operator 'STORE)
				(vm_run_store vm_name arguments)
			)
			((eql operator 'MOVE)
				(vm_run_move vm_name arguments)
			) 
			((eql operator 'ADD)
				(vm_run_add vm_name arguments)
			) 
			((eql operator 'SUB)
				(vm_run_sub vm_name arguments)
			) 
			((eql operator 'MUL)
				(vm_run_mul vm_name arguments)
			) 
			((eql operator 'DIV)
				(vm_run_div vm_name arguments)
			) 
			((eql operator 'INCR)
				(vm_run_incr vm_name arguments)
			) 
			((eql operator 'DECR)
				(vm_run_decr vm_name arguments)
			) 
			((eql operator 'PUSH)
				(vm_run_push vm_name arguments)
			) 
			((eql operator 'POP)
				(vm_run_pop vm_name arguments)
			) 
			((eql operator 'JMP)
				(vm_run_jmp vm_name arguments)
			) 
			((eql operator 'JSR)
				(vm_run_jsr vm_name arguments)
			) 
			((eql operator 'RTN)
				(vm_run_rtn vm_name arguments)
			) 
			((eql operator 'CMP)
				(vm_run_cmp vm_name arguments)
			) 
			((eql operator 'JGT)
				(vm_run_jgt vm_name arguments)
			) 
			((eql operator 'JGE)
				(vm_run_jge vm_name arguments)
			) 
			((eql operator 'JLT)
				(vm_run_jlt vm_name arguments)
			) 
			((eql operator 'JLE)
				(vm_run_jle vm_name arguments)
			) 
			((eql operator 'JEQ)
				(vm_run_jeq vm_name arguments)
			) 
			((eql operator 'JNE)
				(vm_run_jne vm_name arguments)
			) 
			((eql operator 'TEST)
				(vm_run_test vm_name arguments)
			) 
			((eql operator 'JTRUE)
				(vm_run_jtrue vm_name arguments)
			) 
			((eql operator 'JNIL)
				(vm_run_jnil vm_name arguments)
			)
			((eql operator 'HALT)
				(vm_run_halt vm_name arguments)
			)
			((eql operator 'WRITE)
				(progn
					(write (vm_get_value vm_name (car arguments)))
					(write-line " ")
				)
			) 
			((eql operator 'DUMP)
				(progn
					(write (get vm_name 'memory))
					(write-line " ")
				)
			) 
		)
	)
)

(defun vm_run_load (vm_name arguments) 
	(let ( (src (car arguments)) (dest (cadr arguments)) )
		(setf (get vm_name dest) (nth (vm_get_value vm_name src) (get vm_name 'memory)))
	)
)

(defun vm_run_store (vm_name arguments) 
	(let ( (src (car arguments)) (dest (cadr arguments)) )
	(setf (get vm_name 'memory) (array_set (get vm_name 'memory) (vm_get_value vm_name dest) (get vm_name src)))
	)
)

(defun vm_run_move (vm_name arguments) 
	(let ( (src (car arguments)) (dest (cadr arguments)) )
		(setf (get vm_name dest) (vm_get_value vm_name src))
	)
)

(defun vm_run_add (vm_name arguments)
	(let ( 
		(src1 (vm_get_value vm_name (car arguments)))
		(src2 (get vm_name (cadr arguments)))
	)
		(setf (get vm_name (cadr arguments)) (+ src1 src2))
	)
)

(defun vm_run_sub (vm_name arguments)
	(let ( 
		(src1 (vm_get_value vm_name (car arguments)))
		(src2 (get vm_name (cadr arguments)))
	)
		(setf (get vm_name (cadr arguments)) (- src1 src2))
	)
)

(defun vm_run_mul (vm_name arguments)
	(let ( 
		(src1 (vm_get_value vm_name (car arguments)))
		(src2 (get vm_name (cadr arguments)))
	)
		(setf (get vm_name (cadr arguments)) (* src1 src2))
	)
)

(defun vm_run_div (vm_name arguments)
	(let ( 
		(src1 (vm_get_value vm_name (car arguments)))
		(src2 (get vm_name (cadr arguments)))
	)
		(setf (get vm_name (cadr arguments)) (/ src1 src2))
	)
)

(defun vm_run_decr (vm_name arguments)
	(let ( (src (get vm_name (car arguments))))
		(setf (get vm_name (car arguments)) (- src 1))
	)
)

(defun vm_run_incr (vm_name arguments)
	(let ( (src (get vm_name (car arguments))))
		(setf (get vm_name (car arguments)) (+ src 1))
	)
)

(defun vm_run_push (vm_name arguments)
	(let ( (src (vm_get_value vm_name (car arguments))))
		(setf (get vm_name 'memory) (array_set (get vm_name 'memory) (get vm_name 'SP) src))
		(setf (get vm_name 'SP) (+ (get vm_name 'SP) 1))
	)
)

(defun vm_run_pop (vm_name arguments)
	(let ( (dest (car arguments)))
		(setf (get vm_name 'SP) (- (get vm_name 'SP) 1))
		(setf (get vm_name dest) (nth (get vm_name 'SP) (get vm_name 'memory)))
	)
)

(defun vm_run_jmp (vm_name arguments)
	(setf (get vm_name 'PC) (vm_get_adresse_label vm_name (vm_get_address vm_name (car arguments))))
)

(defun vm_run_jsr (vm_name arguments)
	(let ((fp_temp (get vm_name 'FP)) (sp_temp (get vm_name 'SP)))
		(setf (get vm_name 'FP)(- (get vm_name 'SP) 1))
		
		(vm_run_push vm_name (cons(list ':CONST fp_temp) NIL));on empile FP
		(vm_run_push vm_name '(PC));on empile PC
		(vm_run_push vm_name (cons(list ':CONST sp_temp) NIL));on empile SP
		
		(if (map_get (get vm_name 'symbols) (car arguments) )
			(setf (get vm_name 'PC)(vm_get_adresse_label vm_name (vm_get_address vm_name (car arguments))))
			(vm_run_lisp_function (car arguments) vm_name arguments)
		)
	)
)

(defun vm_run_lisp_function (func vm_name arguments)
	(vm_run_move vm_name (list 'FP 'R0))
	(vm_run_load vm_name (list 'R0 'R0))
	
	(let 
		(
			(nb_arguments (vm_get_value vm_name 'R0))
		)
		
		(vm_run_move vm_name 
			(list 
				(list 
					':CONST 
					( if (eq func 'setf)
						(call-setf (car (get_argument_func_lisp vm_name nb_arguments)) (cadr (get_argument_func_lisp vm_name nb_arguments)))
						(apply func (get_argument_func_lisp vm_name nb_arguments))
					)
				) 'R0
			)
		)
		
		(vm_run_rtn vm_name arguments)
	)
)

(defun call-setf (key value) 
	(eval `(setf ,key ,value))
)

(defun get_argument_func_lisp (vm_name nb_arguments)
	(if (= nb_arguments 0)
		'()
		(progn
			(vm_run_move vm_name (list 'FP 'R0))
			(vm_run_move vm_name (list (list':CONST nb_arguments) 'R1))
			(vm_run_sub vm_name (list 'R0 'R1))
			(vm_run_move vm_name (list 'R1 'R0))
			(vm_run_load vm_name (list 'R0 'R0))
			(append 
				(list (vm_get_value vm_name 'R0))
				(get_argument_func_lisp vm_name (- nb_arguments 1))
			)
		)
	)
)

(defun vm_run_rtn (vm_name arguments)
	(setf (get vm_name 'SP)(nth (+ (get vm_name 'FP) 3) (get vm_name 'memory)))
	(setf (get vm_name 'PC)(nth (+ (get vm_name 'FP) 2) (get vm_name 'memory)))
	(setf (get vm_name 'FP)(nth (+ (get vm_name 'FP) 1) (get vm_name 'memory)))
)

(defun vm_run_cmp (vm_name arguments)
	(let ( 
		(src1 (vm_get_value vm_name (car arguments)))
		(src2 (vm_get_value vm_name (cadr arguments)))
	)
		(if (< src1 src2)
			(progn
				(setf (get vm_name 'INF) T)
				(setf (get vm_name 'EQU) NIL)
				(setf (get vm_name 'SUP) NIL)
			)
			(if (> src1 src2)
				(progn
					(setf (get vm_name 'INF) NIL)
					(setf (get vm_name 'EQU) NIL)
					(setf (get vm_name 'SUP) T)
				)
				(progn
					(setf (get vm_name 'INF) NIL)
					(setf (get vm_name 'EQU) T)
					(setf (get vm_name 'SUP) NIL)
				)
			)
		)
	)
)

(defun vm_run_jgt (vm_name arguments)
	(if  (get vm_name 'SUP)
		(setf (get vm_name 'PC) (vm_get_adresse_label vm_name  (vm_get_address vm_name (car arguments))))
		NIL
	)
)

(defun vm_run_jge (vm_name arguments)
	(if (or (get vm_name 'EQU) (get vm_name 'SUP))
		(setf (get vm_name 'PC) (vm_get_adresse_label vm_name  (vm_get_address vm_name (car arguments))))
		NIL
	)
)

(defun vm_run_jlt (vm_name arguments)
	(if (get vm_name 'INF)
		(setf (get vm_name 'PC) (vm_get_adresse_label vm_name  (vm_get_address vm_name (car arguments))))
		NIL
	)
)

(defun vm_run_jle (vm_name arguments)
	(if (or (get vm_name 'EQU) (get vm_name 'INF))
		(setf (get vm_name 'PC) (vm_get_adresse_label vm_name  (vm_get_address vm_name (car arguments))))
		NIL
	)
)

(defun vm_run_jeq (vm_name arguments)
	(if (get vm_name 'EQU)
		(setf (get vm_name 'PC) (vm_get_adresse_label vm_name  (vm_get_address vm_name (car arguments))))
		NIL
	)
)

(defun vm_run_jne (vm_name arguments)
	(if (not (get vm_name 'EQU))
		(setf (get vm_name 'PC) (vm_get_adresse_label vm_name  (vm_get_address vm_name (car arguments))))
		NIL
	)
)

(defun vm_run_test (vm_name arguments)
	(if (null (get vm_name (car arguments)))
		(setf (get vm_name 'FNIL) T)
		(setf (get vm_name 'FNIL) NIL)
	)
)

(defun vm_run_jtrue (vm_name arguments)
	(if (not (get vm_name 'FNIL))
		(setf (get vm_name 'PC) (vm_get_adresse_label vm_name  (vm_get_address vm_name (car arguments))))
		NIL
	)
)

(defun vm_run_jnil (vm_name arguments)
	(if  (get vm_name 'FNIL)
		(setf (get vm_name 'PC) (vm_get_adresse_label vm_name  (vm_get_address vm_name (car arguments))))
		NIL
	)
)

(defun vm_run_halt (vm_name arguments)
	(setf(get vm_name 'PC )(get vm_name 'EC))
)

(defun vm_get_adresse_label(vm_name label)
	(+ (map_get (get vm_name 'symbols) label) (get vm_name 'CP))
)

(vm_create 'Roger 10000)
(vm_load 'Roger '(
	(MOVE (:CONST "Hello World !") R0)
	(PUSH R0)
	(PUSH (:CONST 1))
	(JSR WRITE)
))

(write (vm_run 'Roger))
(write-line "============")
