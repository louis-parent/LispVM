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
	(write-line "VM Created")
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
	(write-line "VM Loaded")
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
    	'()
        (progn
            (array_set (get vm_name 'memory) (get vm_name 'EC) (car instructions))
            (setf (get vm_name 'EC) (+ 1 (get vm_name 'EC)))
            (vm_load_code vm_name (cdr instructions))
        )
    )
)

(defun array_set (l n value)
    (setf (nth n l) value)
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
                        (call-setf 
                        	(replace_offset_to_runtime_value vm_name (car (get_argument_func_lisp vm_name nb_arguments)))
                        	(cadr (get_argument_func_lisp vm_name nb_arguments))
                        )
                        
                        (apply func (get_argument_func_lisp vm_name nb_arguments))
                    )
                ) 'R0
            )
        )
        (vm_run_rtn vm_name arguments)
    )
)

(defun replace_offset_to_runtime_value (vm_name instruction)
    (if (null instruction)
        '()
        (if (and (listp (car instruction)) (eq (caar instruction) ':VAR))
            (let (
            		(value (nth
	    				(+
	    					(nth 
		    					(+ (get vm_name 'FP) 1)
		    					(get vm_name 'memory)
		    				)
		    				(cadar instruction)
		    			)
	    				(get vm_name 'memory)
	    			))
            	)

            	(cons
            	    (eval `(quote ',value))
	            	(replace_offset_to_runtime_value vm_name (cdr instruction))
	            )
            )

            (cons
            	(car instruction)
            	(replace_offset_to_runtime_value vm_name (cdr instruction))
            )
        )
    )
)

(defun call-setf (key value)
    (if (listp value)
        (eval `(setf ,key ',value))
        (eval `(setf ,key ,value))
    )
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

(vm_create 'Roger 50000)
(vm_load 'Roger '(

(JMP SKIP_FUNCTION0) (LABEL VM_CREATE) (PUSH (:CONST (GET (:VAR -2) (QUOTE MEMORY)))) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR MAKE-LIST) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST 0.9) R0) (PUSH R0) (POP R0) (POP R1) (MUL R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR FLOOR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE CP)))) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST 0.9) R0) (PUSH R0) (POP R0) (POP R1) (MUL R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR FLOOR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE BP)))) (MOVE (:CONST 0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE FP)))) (MOVE (:CONST 0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE SP)))) (MOVE (:CONST 0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE EC)))) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST 0.9) R0) (PUSH R0) (POP R0) (POP R1) (MUL R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR FLOOR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE ES)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST CP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST 1) R0) (PUSH R0) (POP R0) (POP R1) (SUB R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE R0)))) (MOVE (:CONST 0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE R1)))) (MOVE (:CONST 0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE R2)))) (MOVE (:CONST 0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE STOP)))) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE INF)))) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE EQU)))) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE SUP)))) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE FNIL)))) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE SYMBOLS)))) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (MOVE (:CONST "VM Created") R0) (PUSH R0) (PUSH (:CONST 1)) (JSR WRITE-LINE) (POP R1) (POP R1) (RTN) (LABEL SKIP_FUNCTION0) (JMP SKIP_FUNCTION1) (LABEL VM_LOAD) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_LOAD_CODE) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE SYMBOLS)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST SYMBOLS) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST CP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 1)) (JSR FLOOR) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MEMORY) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR NTHCDR) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST 0) R0) (PUSH R0) (PUSH (:CONST 3)) (JSR VM_LOAD_SYMBOLS) (POP R1) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR APPEND) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (MOVE (:CONST "VM Loaded") R0) (PUSH R0) (PUSH (:CONST 1)) (JSR WRITE-LINE) (POP R1) (POP R1) (RTN) (LABEL SKIP_FUNCTION1) (JMP SKIP_FUNCTION2) (LABEL VM_LOAD_SYMBOLS) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (TEST R0) (JNIL CMP_NULL_TRUE0) (MOVE (:CONST NIL) R0) (JMP CMP_NULL_END0) (LABEL CMP_NULL_TRUE0) (MOVE (:CONST T) R0) (LABEL CMP_NULL_END0) (TEST R0) (JTRUE IF_TRUE1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAAR) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST LABEL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQ) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE0) (MOVE FP R1) (ADD (:CONST -3) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CDR) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST 1) R0) (PUSH R0) (POP R0) (POP R1) (ADD R1 R0) (PUSH R0) (PUSH (:CONST 3)) (JSR VM_LOAD_SYMBOLS) (POP R1) (POP R1) (POP R1) (POP R1) (JMP IF_END0) (LABEL IF_TRUE0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CADAR) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR CONS) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR CONS) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -3) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CDR) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST 1) R0) (PUSH R0) (POP R0) (POP R1) (ADD R1 R0) (PUSH R0) (PUSH (:CONST 3)) (JSR VM_LOAD_SYMBOLS) (POP R1) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR CONS) (POP R1) (POP R1) (POP R1) (JMP IF_END0) (LABEL IF_END0) (JMP IF_END1) (LABEL IF_TRUE1) (MOVE (:CONST NIL) R0) (JMP IF_END1) (LABEL IF_END1) (RTN) (LABEL SKIP_FUNCTION2) (JMP SKIP_FUNCTION3) (LABEL VM_LOAD_CODE) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (TEST R0) (JNIL CMP_NULL_TRUE1) (MOVE (:CONST NIL) R0) (JMP CMP_NULL_END1) (LABEL CMP_NULL_TRUE1) (MOVE (:CONST T) R0) (LABEL CMP_NULL_END1) (TEST R0) (JTRUE IF_TRUE2) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MEMORY) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST EC) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 3)) (JSR ARRAY_SET) (POP R1) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE EC)))) (MOVE (:CONST 1) R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST EC) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (POP R0) (POP R1) (ADD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CDR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_LOAD_CODE) (POP R1) (POP R1) (POP R1) (JMP IF_END2) (LABEL IF_TRUE2) (MOVE (:CONST NIL) R0) (JMP IF_END2) (LABEL IF_END2) (RTN) (LABEL SKIP_FUNCTION3) (JMP SKIP_FUNCTION4) (LABEL ARRAY_SET) (PUSH (:CONST (NTH (:VAR -2) (:VAR -3)))) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -3) R1) (LOAD R1 R0) (RTN) (LABEL SKIP_FUNCTION4) (JMP SKIP_FUNCTION5) (LABEL MAP_GET) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (TEST R0) (JNIL CMP_NULL_TRUE2) (MOVE (:CONST NIL) R0) (JMP CMP_NULL_END2) (LABEL CMP_NULL_TRUE2) (MOVE (:CONST T) R0) (LABEL CMP_NULL_END2) (TEST R0) (JTRUE IF_TRUE4) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAAR) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQ) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE3) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CDR) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR MAP_GET) (POP R1) (POP R1) (POP R1) (JMP IF_END3) (LABEL IF_TRUE3) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CADAR) (POP R1) (POP R1) (JMP IF_END3) (LABEL IF_END3) (JMP IF_END4) (LABEL IF_TRUE4) (MOVE (:CONST NIL) R0) (JMP IF_END4) (LABEL IF_END4) (RTN) (LABEL SKIP_FUNCTION5) (JMP SKIP_FUNCTION6) (LABEL VM_GET_VALUE) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR LISTP) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE5) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (JMP IF_END5) (LABEL IF_TRUE5) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CADR) (POP R1) (POP R1) (JMP IF_END5) (LABEL IF_END5) (RTN) (LABEL SKIP_FUNCTION6) (JMP SKIP_FUNCTION7) (LABEL VM_GET_ADDRESS) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR LISTP) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE6) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (JMP IF_END6) (LABEL IF_TRUE6) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CADR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (JMP IF_END6) (LABEL IF_END6) (RTN) (LABEL SKIP_FUNCTION7) (JMP SKIP_FUNCTION8) (LABEL VM_RUN) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST PC) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST EC) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (POP R1) (CMP R1 R0) (JEQ CMP_EQUAL_TRUE0) (MOVE (:CONST NIL) R0) (JMP CMP_EQUAL_END0) (LABEL CMP_EQUAL_TRUE0) (MOVE (:CONST T) R0) (LABEL CMP_EQUAL_END0) (TEST R0) (JTRUE IF_TRUE7) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR GET_CURRENT_INSTRUCTION) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST (GET (:VAR -1) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST PC) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST 1) R0) (PUSH R0) (POP R0) (POP R1) (ADD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_INSTRUCTION) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR VM_RUN) (POP R1) (POP R1) (POP R2) (JMP IF_END7) (LABEL IF_TRUE7) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST R0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (JMP IF_END7) (LABEL IF_END7) (RTN) (LABEL SKIP_FUNCTION8) (JMP SKIP_FUNCTION9) (LABEL GET_CURRENT_INSTRUCTION) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST PC) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MEMORY) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR NTH) (POP R1) (POP R1) (POP R1) (RTN) (LABEL SKIP_FUNCTION9) (JMP SKIP_FUNCTION10) (LABEL VM_RUN_INSTRUCTION) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CDR) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST LOAD) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE34) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST STORE) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE33) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MOVE) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE32) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST ADD) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE31) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST SUB) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE30) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MUL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE29) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST DIV) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE28) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST INCR) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE27) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST DECR) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE26) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST PUSH) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE25) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST POP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE24) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST JMP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE23) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST JSR) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE22) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST RTN) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE21) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST CMP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE20) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST JGT) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE19) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST JGE) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE18) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST JLT) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE17) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST JLE) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE16) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST JEQ) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE15) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST JNE) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE14) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST TEST) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE13) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST JTRUE) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE12) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST JNIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE11) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST HALT) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE10) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST WRITE) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE9) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST DUMP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQL) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE8) (MOVE (:CONST NIL) R0) (JMP IF_END8) (LABEL IF_TRUE8) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MEMORY) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 1)) (JSR WRITE) (POP R1) (POP R1) (MOVE (:CONST " ") R0) (PUSH R0) (PUSH (:CONST 1)) (JSR WRITE-LINE) (POP R1) (POP R1) (JMP IF_END8) (LABEL IF_END8) (JMP IF_END9) (LABEL IF_TRUE9) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 1)) (JSR WRITE) (POP R1) (POP R1) (MOVE (:CONST " ") R0) (PUSH R0) (PUSH (:CONST 1)) (JSR WRITE-LINE) (POP R1) (POP R1) (JMP IF_END9) (LABEL IF_END9) (JMP IF_END10) (LABEL IF_TRUE10) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_HALT) (POP R1) (POP R1) (POP R1) (JMP IF_END10) (LABEL IF_END10) (JMP IF_END11) (LABEL IF_TRUE11) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_JNIL) (POP R1) (POP R1) (POP R1) (JMP IF_END11) (LABEL IF_END11) (JMP IF_END12) (LABEL IF_TRUE12) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_JTRUE) (POP R1) (POP R1) (POP R1) (JMP IF_END12) (LABEL IF_END12) (JMP IF_END13) (LABEL IF_TRUE13) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_TEST) (POP R1) (POP R1) (POP R1) (JMP IF_END13) (LABEL IF_END13) (JMP IF_END14) (LABEL IF_TRUE14) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_JNE) (POP R1) (POP R1) (POP R1) (JMP IF_END14) (LABEL IF_END14) (JMP IF_END15) (LABEL IF_TRUE15) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_JEQ) (POP R1) (POP R1) (POP R1) (JMP IF_END15) (LABEL IF_END15) (JMP IF_END16) (LABEL IF_TRUE16) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_JLE) (POP R1) (POP R1) (POP R1) (JMP IF_END16) (LABEL IF_END16) (JMP IF_END17) (LABEL IF_TRUE17) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_JLT) (POP R1) (POP R1) (POP R1) (JMP IF_END17) (LABEL IF_END17) (JMP IF_END18) (LABEL IF_TRUE18) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_JGE) (POP R1) (POP R1) (POP R1) (JMP IF_END18) (LABEL IF_END18) (JMP IF_END19) (LABEL IF_TRUE19) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_JGT) (POP R1) (POP R1) (POP R1) (JMP IF_END19) (LABEL IF_END19) (JMP IF_END20) (LABEL IF_TRUE20) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_CMP) (POP R1) (POP R1) (POP R1) (JMP IF_END20) (LABEL IF_END20) (JMP IF_END21) (LABEL IF_TRUE21) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_RTN) (POP R1) (POP R1) (POP R1) (JMP IF_END21) (LABEL IF_END21) (JMP IF_END22) (LABEL IF_TRUE22) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_JSR) (POP R1) (POP R1) (POP R1) (JMP IF_END22) (LABEL IF_END22) (JMP IF_END23) (LABEL IF_TRUE23) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_JMP) (POP R1) (POP R1) (POP R1) (JMP IF_END23) (LABEL IF_END23) (JMP IF_END24) (LABEL IF_TRUE24) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_POP) (POP R1) (POP R1) (POP R1) (JMP IF_END24) (LABEL IF_END24) (JMP IF_END25) (LABEL IF_TRUE25) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_PUSH) (POP R1) (POP R1) (POP R1) (JMP IF_END25) (LABEL IF_END25) (JMP IF_END26) (LABEL IF_TRUE26) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_DECR) (POP R1) (POP R1) (POP R1) (JMP IF_END26) (LABEL IF_END26) (JMP IF_END27) (LABEL IF_TRUE27) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_INCR) (POP R1) (POP R1) (POP R1) (JMP IF_END27) (LABEL IF_END27) (JMP IF_END28) (LABEL IF_TRUE28) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_DIV) (POP R1) (POP R1) (POP R1) (JMP IF_END28) (LABEL IF_END28) (JMP IF_END29) (LABEL IF_TRUE29) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_MUL) (POP R1) (POP R1) (POP R1) (JMP IF_END29) (LABEL IF_END29) (JMP IF_END30) (LABEL IF_TRUE30) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_SUB) (POP R1) (POP R1) (POP R1) (JMP IF_END30) (LABEL IF_END30) (JMP IF_END31) (LABEL IF_TRUE31) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_ADD) (POP R1) (POP R1) (POP R1) (JMP IF_END31) (LABEL IF_END31) (JMP IF_END32) (LABEL IF_TRUE32) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_MOVE) (POP R1) (POP R1) (POP R1) (JMP IF_END32) (LABEL IF_END32) (JMP IF_END33) (LABEL IF_TRUE33) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_STORE) (POP R1) (POP R1) (POP R1) (JMP IF_END33) (LABEL IF_END33) (JMP IF_END34) (LABEL IF_TRUE34) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_LOAD) (POP R1) (POP R1) (POP R1) (JMP IF_END34) (LABEL IF_END34) (POP R2) (POP R2) (RTN) (LABEL SKIP_FUNCTION10) (JMP SKIP_FUNCTION11) (LABEL VM_RUN_LOAD) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CADR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST (GET (:VAR -2) (:VAR 5)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MEMORY) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR NTH) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (POP R2) (POP R2) (RTN) (LABEL SKIP_FUNCTION11) (JMP SKIP_FUNCTION12) (LABEL VM_RUN_STORE) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CADR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST (GET (:VAR -2) (QUOTE MEMORY)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MEMORY) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 3)) (JSR ARRAY_SET) (POP R1) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (POP R2) (POP R2) (RTN) (LABEL SKIP_FUNCTION12) (JMP SKIP_FUNCTION13) (LABEL VM_RUN_MOVE) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CADR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST (GET (:VAR -2) (:VAR 5)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (POP R2) (POP R2) (RTN) (LABEL SKIP_FUNCTION13) (JMP SKIP_FUNCTION14) (LABEL VM_RUN_ADD) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CADR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST (GET (:VAR -2) (CADR ARGUMENTS)))) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (POP R0) (POP R1) (ADD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (POP R2) (POP R2) (RTN) (LABEL SKIP_FUNCTION14) (JMP SKIP_FUNCTION15) (LABEL VM_RUN_SUB) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CADR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST (GET (:VAR -2) (CADR ARGUMENTS)))) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (POP R0) (POP R1) (SUB R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (POP R2) (POP R2) (RTN) (LABEL SKIP_FUNCTION15) (JMP SKIP_FUNCTION16) (LABEL VM_RUN_MUL) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CADR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST (GET (:VAR -2) (CADR ARGUMENTS)))) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (POP R0) (POP R1) (MUL R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (POP R2) (POP R2) (RTN) (LABEL SKIP_FUNCTION16) (JMP SKIP_FUNCTION17) (LABEL VM_RUN_DIV) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CADR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST (GET (:VAR -2) (CADR ARGUMENTS)))) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (POP R0) (POP R1) (DIV R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (POP R2) (POP R2) (RTN) (LABEL SKIP_FUNCTION17) (JMP SKIP_FUNCTION18) (LABEL VM_RUN_DECR) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST (GET (:VAR -2) (CAR ARGUMENTS)))) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST 1) R0) (PUSH R0) (POP R0) (POP R1) (SUB R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (POP R2) (RTN) (LABEL SKIP_FUNCTION18) (JMP SKIP_FUNCTION19) (LABEL VM_RUN_INCR) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST (GET (:VAR -2) (CAR ARGUMENTS)))) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST 1) R0) (PUSH R0) (POP R0) (POP R1) (ADD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (POP R2) (RTN) (LABEL SKIP_FUNCTION19) (JMP SKIP_FUNCTION20) (LABEL VM_RUN_PUSH) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST (GET (:VAR -2) (QUOTE MEMORY)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MEMORY) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST SP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 3)) (JSR ARRAY_SET) (POP R1) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE SP)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST SP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST 1) R0) (PUSH R0) (POP R0) (POP R1) (ADD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (POP R2) (RTN) (LABEL SKIP_FUNCTION20) (JMP SKIP_FUNCTION21) (LABEL VM_RUN_POP) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST (GET (:VAR -2) (QUOTE SP)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST SP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST 1) R0) (PUSH R0) (POP R0) (POP R1) (SUB R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (:VAR 4)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST SP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MEMORY) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR NTH) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (POP R2) (RTN) (LABEL SKIP_FUNCTION21) (JMP SKIP_FUNCTION22) (LABEL VM_RUN_JMP) (PUSH (:CONST (GET (:VAR -2) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADDRESS) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADRESSE_LABEL) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (RTN) (LABEL SKIP_FUNCTION22) (JMP SKIP_FUNCTION23) (LABEL VM_RUN_JSR) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST FP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST SP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST (GET (:VAR -2) (QUOTE FP)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST SP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST 1) R0) (PUSH R0) (POP R0) (POP R1) (SUB R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST :CONST) R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR CONS) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_PUSH) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST (PC)) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_PUSH) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST :CONST) R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR CONS) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_PUSH) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST SYMBOLS) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR MAP_GET) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE35) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 3)) (JSR VM_RUN_LISP_FUNCTION) (POP R1) (POP R1) (POP R1) (POP R1) (JMP IF_END35) (LABEL IF_TRUE35) (PUSH (:CONST (GET (:VAR -2) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADDRESS) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADRESSE_LABEL) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END35) (LABEL IF_END35) (POP R2) (POP R2) (RTN) (LABEL SKIP_FUNCTION23) (JMP SKIP_FUNCTION24) (LABEL VM_RUN_LISP_FUNCTION) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST FP) R0) (PUSH R0) (MOVE (:CONST R0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_MOVE) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST R0) R0) (PUSH R0) (MOVE (:CONST R0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_LOAD) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST R0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST :CONST) R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -3) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST SETF) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQ) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE36) (MOVE FP R1) (ADD (:CONST -3) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET_ARGUMENT_FUNC_LISP) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR APPLY) (POP R1) (POP R1) (POP R1) (JMP IF_END36) (LABEL IF_TRUE36) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET_ARGUMENT_FUNC_LISP) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR REPLACE_OFFSET_TO_RUNTIME_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET_ARGUMENT_FUNC_LISP) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 1)) (JSR CADR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR CALL-SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END36) (LABEL IF_END36) (PUSH R0) (PUSH (:CONST 2)) (JSR LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST R0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_MOVE) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_RTN) (POP R1) (POP R1) (POP R1) (POP R2) (RTN) (LABEL SKIP_FUNCTION24) (JMP SKIP_FUNCTION25) (LABEL REPLACE_OFFSET_TO_RUNTIME_VALUE) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (TEST R0) (JNIL CMP_NULL_TRUE3) (MOVE (:CONST NIL) R0) (JMP CMP_NULL_END3) (LABEL CMP_NULL_TRUE3) (MOVE (:CONST T) R0) (LABEL CMP_NULL_END3) (TEST R0) (JTRUE IF_TRUE38) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 1)) (JSR LISTP) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAAR) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST :VAR) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR EQ) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR AND) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE37) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CDR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR REPLACE_OFFSET_TO_RUNTIME_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR CONS) (POP R1) (POP R1) (POP R1) (JMP IF_END37) (LABEL IF_TRUE37) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST FP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST 1) R0) (PUSH R0) (POP R0) (POP R1) (ADD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MEMORY) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR NTH) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CADAR) (POP R1) (POP R1) (PUSH R0) (POP R0) (POP R1) (ADD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MEMORY) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR NTH) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST QUOTE) R0) (PUSH R0) (MOVE (:CONST QUOTE) R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SYSTEM::BQ-LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SYSTEM::BQ-LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 1)) (JSR EVAL) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CDR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR REPLACE_OFFSET_TO_RUNTIME_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR CONS) (POP R1) (POP R1) (POP R1) (POP R2) (JMP IF_END37) (LABEL IF_END37) (JMP IF_END38) (LABEL IF_TRUE38) (MOVE (:CONST NIL) R0) (JMP IF_END38) (LABEL IF_END38) (RTN) (LABEL SKIP_FUNCTION25) (JMP SKIP_FUNCTION26) (LABEL CALL-SETF) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR LISTP) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE39) (MOVE (:CONST SETF) R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 3)) (JSR SYSTEM::BQ-LIST) (POP R1) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 1)) (JSR EVAL) (POP R1) (POP R1) (JMP IF_END39) (LABEL IF_TRUE39) (MOVE (:CONST SETF) R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST QUOTE) R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SYSTEM::BQ-LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 3)) (JSR SYSTEM::BQ-LIST) (POP R1) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 1)) (JSR EVAL) (POP R1) (POP R1) (JMP IF_END39) (LABEL IF_END39) (RTN) (LABEL SKIP_FUNCTION26) (JMP SKIP_FUNCTION27) (LABEL GET_ARGUMENT_FUNC_LISP) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST 0) R0) (POP R1) (CMP R1 R0) (JEQ CMP_EQUAL_TRUE1) (MOVE (:CONST NIL) R0) (JMP CMP_EQUAL_END1) (LABEL CMP_EQUAL_TRUE1) (MOVE (:CONST T) R0) (LABEL CMP_EQUAL_END1) (TEST R0) (JTRUE IF_TRUE40) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST FP) R0) (PUSH R0) (MOVE (:CONST R0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_MOVE) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST :CONST) R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST R1) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_MOVE) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST R0) R0) (PUSH R0) (MOVE (:CONST R1) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_SUB) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST R1) R0) (PUSH R0) (MOVE (:CONST R0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_MOVE) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST R0) R0) (PUSH R0) (MOVE (:CONST R0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR LIST) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_RUN_LOAD) (POP R1) (POP R1) (POP R1) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST R0) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 1)) (JSR LIST) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST 1) R0) (PUSH R0) (POP R0) (POP R1) (SUB R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET_ARGUMENT_FUNC_LISP) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR APPEND) (POP R1) (POP R1) (POP R1) (JMP IF_END40) (LABEL IF_TRUE40) (MOVE (:CONST NIL) R0) (JMP IF_END40) (LABEL IF_END40) (RTN) (LABEL SKIP_FUNCTION27) (JMP SKIP_FUNCTION28) (LABEL VM_RUN_RTN) (PUSH (:CONST (GET (:VAR -2) (QUOTE SP)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST FP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST 3) R0) (PUSH R0) (POP R0) (POP R1) (ADD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MEMORY) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR NTH) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST FP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST 2) R0) (PUSH R0) (POP R0) (POP R1) (ADD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MEMORY) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR NTH) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE FP)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST FP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE (:CONST 1) R0) (PUSH R0) (POP R0) (POP R1) (ADD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST MEMORY) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR NTH) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (RTN) (LABEL SKIP_FUNCTION28) (JMP SKIP_FUNCTION29) (LABEL VM_RUN_CMP) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CADR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_VALUE) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (POP R1) (CMP R1 R0) (JLT CMP_LOWER_TRUE0) (MOVE (:CONST NIL) R0) (JMP CMP_LOWER_END0) (LABEL CMP_LOWER_TRUE0) (MOVE (:CONST T) R0) (LABEL CMP_LOWER_END0) (TEST R0) (JTRUE IF_TRUE42) (MOVE FP R1) (ADD (:CONST 4) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST 5) R1) (LOAD R1 R0) (POP R1) (CMP R1 R0) (JGT CMP_GREATER_TRUE0) (MOVE (:CONST NIL) R0) (JMP CMP_GREATER_END0) (LABEL CMP_GREATER_TRUE0) (MOVE (:CONST T) R0) (LABEL CMP_GREATER_END0) (TEST R0) (JTRUE IF_TRUE41) (PUSH (:CONST (GET (:VAR -2) (QUOTE INF)))) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE EQU)))) (MOVE (:CONST T) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE SUP)))) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END41) (LABEL IF_TRUE41) (PUSH (:CONST (GET (:VAR -2) (QUOTE INF)))) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE EQU)))) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE SUP)))) (MOVE (:CONST T) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END41) (LABEL IF_END41) (JMP IF_END42) (LABEL IF_TRUE42) (PUSH (:CONST (GET (:VAR -2) (QUOTE INF)))) (MOVE (:CONST T) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE EQU)))) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (PUSH (:CONST (GET (:VAR -2) (QUOTE SUP)))) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END42) (LABEL IF_END42) (POP R2) (POP R2) (RTN) (LABEL SKIP_FUNCTION29) (JMP SKIP_FUNCTION30) (LABEL VM_RUN_JGT) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST SUP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE43) (MOVE (:CONST NIL) R0) (JMP IF_END43) (LABEL IF_TRUE43) (PUSH (:CONST (GET (:VAR -2) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADDRESS) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADRESSE_LABEL) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END43) (LABEL IF_END43) (RTN) (LABEL SKIP_FUNCTION30) (JMP SKIP_FUNCTION31) (LABEL VM_RUN_JGE) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST EQU) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST SUP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR OR) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE44) (MOVE (:CONST NIL) R0) (JMP IF_END44) (LABEL IF_TRUE44) (PUSH (:CONST (GET (:VAR -2) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADDRESS) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADRESSE_LABEL) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END44) (LABEL IF_END44) (RTN) (LABEL SKIP_FUNCTION31) (JMP SKIP_FUNCTION32) (LABEL VM_RUN_JLT) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST INF) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE45) (MOVE (:CONST NIL) R0) (JMP IF_END45) (LABEL IF_TRUE45) (PUSH (:CONST (GET (:VAR -2) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADDRESS) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADRESSE_LABEL) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END45) (LABEL IF_END45) (RTN) (LABEL SKIP_FUNCTION32) (JMP SKIP_FUNCTION33) (LABEL VM_RUN_JLE) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST EQU) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST INF) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR OR) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE46) (MOVE (:CONST NIL) R0) (JMP IF_END46) (LABEL IF_TRUE46) (PUSH (:CONST (GET (:VAR -2) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADDRESS) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADRESSE_LABEL) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END46) (LABEL IF_END46) (RTN) (LABEL SKIP_FUNCTION33) (JMP SKIP_FUNCTION34) (LABEL VM_RUN_JEQ) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST EQU) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE47) (MOVE (:CONST NIL) R0) (JMP IF_END47) (LABEL IF_TRUE47) (PUSH (:CONST (GET (:VAR -2) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADDRESS) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADRESSE_LABEL) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END47) (LABEL IF_END47) (RTN) (LABEL SKIP_FUNCTION34) (JMP SKIP_FUNCTION35) (LABEL VM_RUN_JNE) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST EQU) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE CMP_NULL_TRUE4) (MOVE (:CONST T) R0) (JMP CMP_NULL_END4) (LABEL CMP_NULL_TRUE4) (MOVE (:CONST NIL) R0) (LABEL CMP_NULL_END4) (TEST R0) (JTRUE IF_TRUE48) (MOVE (:CONST NIL) R0) (JMP IF_END48) (LABEL IF_TRUE48) (PUSH (:CONST (GET (:VAR -2) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADDRESS) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADRESSE_LABEL) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END48) (LABEL IF_END48) (RTN) (LABEL SKIP_FUNCTION35) (JMP SKIP_FUNCTION36) (LABEL VM_RUN_TEST) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (TEST R0) (JNIL CMP_NULL_TRUE5) (MOVE (:CONST NIL) R0) (JMP CMP_NULL_END5) (LABEL CMP_NULL_TRUE5) (MOVE (:CONST T) R0) (LABEL CMP_NULL_END5) (TEST R0) (JTRUE IF_TRUE49) (PUSH (:CONST (GET (:VAR -2) (QUOTE FNIL)))) (MOVE (:CONST NIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END49) (LABEL IF_TRUE49) (PUSH (:CONST (GET (:VAR -2) (QUOTE FNIL)))) (MOVE (:CONST T) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END49) (LABEL IF_END49) (RTN) (LABEL SKIP_FUNCTION36) (JMP SKIP_FUNCTION37) (LABEL VM_RUN_JTRUE) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST FNIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE CMP_NULL_TRUE6) (MOVE (:CONST T) R0) (JMP CMP_NULL_END6) (LABEL CMP_NULL_TRUE6) (MOVE (:CONST NIL) R0) (LABEL CMP_NULL_END6) (TEST R0) (JTRUE IF_TRUE50) (MOVE (:CONST NIL) R0) (JMP IF_END50) (LABEL IF_TRUE50) (PUSH (:CONST (GET (:VAR -2) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADDRESS) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADRESSE_LABEL) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END50) (LABEL IF_END50) (RTN) (LABEL SKIP_FUNCTION37) (JMP SKIP_FUNCTION38) (LABEL VM_RUN_JNIL) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST FNIL) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (TEST R0) (JTRUE IF_TRUE51) (MOVE (:CONST NIL) R0) (JMP IF_END51) (LABEL IF_TRUE51) (PUSH (:CONST (GET (:VAR -2) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 1)) (JSR CAR) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADDRESS) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_GET_ADRESSE_LABEL) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (JMP IF_END51) (LABEL IF_END51) (RTN) (LABEL SKIP_FUNCTION38) (JMP SKIP_FUNCTION39) (LABEL VM_RUN_HALT) (PUSH (:CONST (GET (:VAR -2) (QUOTE PC)))) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST EC) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (PUSH (:CONST 2)) (JSR SETF) (POP R1) (POP R1) (POP R1) (RTN) (LABEL SKIP_FUNCTION39) (JMP SKIP_FUNCTION40) (LABEL VM_GET_ADRESSE_LABEL) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST SYMBOLS) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -1) R1) (LOAD R1 R0) (PUSH R0) (PUSH (:CONST 2)) (JSR MAP_GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (MOVE FP R1) (ADD (:CONST -2) R1) (LOAD R1 R0) (PUSH R0) (MOVE (:CONST CP) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR GET) (POP R1) (POP R1) (POP R1) (PUSH R0) (POP R0) (POP R1) (ADD R1 R0) (RTN) (LABEL SKIP_FUNCTION40) (MOVE (:CONST ALBERT) R0) (PUSH R0) (MOVE (:CONST 10000) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_CREATE) (POP R1) (POP R1) (POP R1) (MOVE (:CONST ALBERT) R0) (PUSH R0) (MOVE (:CONST ((WRITE (:CONST 5)))) R0) (PUSH R0) (PUSH (:CONST 2)) (JSR VM_LOAD) (POP R1) (POP R1) (POP R1) (MOVE (:CONST "VM RUNNING") R0) (PUSH R0) (PUSH (:CONST 1)) (JSR WRITE-LINE) (POP R1) (POP R1) (MOVE (:CONST ALBERT) R0) (PUSH R0) (PUSH (:CONST 1)) (JSR VM_RUN) (POP R1) (POP R1)

))

(write-line "VM RUNNING")
(vm_run 'Roger)

