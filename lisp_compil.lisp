(defun compile_lisp (instructions) (compile_instructions instructions '((I (:VAR 4)))))

(defun compile_instructions (instructions environement)
    (if (null instructions)
        NIL
        (let (
                (left (car instructions))
                (right (cdr instructions))
                (bytecode '())
            )
            
            (append
                (append bytecode
                    (list
                        (if (atom left)
                            (compile_atom left right environement)
                            (compile_instructions left environement)
                        )
                    )
                )
                (if (atom right)
                    (compile_atom right NIL environement)
                    (compile_instructions right environement)
                )
            )
        )
    )
)

(defun compile_atom (atom next environement)
    (if (or (null atom) (numberp atom))
        atom
        (cond
            ((eq atom 'DEFUN)
                (compile_defun next environement)
            )
            (T atom)
        )
    )
)

(defun compile_defun (func environement)
    (let (
            (name (car func))
            (args (cadr func))
            (body (cddr func))
            (environement (append environement (list (cons (car func) (cons ':FUNC (compile_defun_args (cadr func)))))))
        )
        
        (cons
            (list 'LABEL name)
            (compile_instructions body environement)
        )
    )
)

(defun compile_defun_args (args)
    (if (null args)
        '()
        (let (
                (size (length args))
            )
            (cons 
                (list (car args) (* size -1))
                (compile_defun_args (cdr args))
            )
        )
    )
)

;(compile_lisp 
;    '(
;        (defun fact (n p)  (if (= n 1) 1 (* n (fact (- n 1)))))
;        (fact 2)
;    )
;)
