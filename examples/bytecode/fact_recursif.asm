(PUSH (:CONST 5))
    (PUSH (:CONST 1))
    (JSR fact)
    (HALT)
    
    (LABEL fact)
    (MOVE FP R0)
    (MOVE (:CONST -1) R1)
    (ADD R0 R1)
    (LOAD R1 R1)
    
    (MOVE (:CONST 1) R0)
    (CMP R1 (:CONST 1))
    
    
    (JEQ RTN_FACT)
    
    (MOVE R1 R2)
    (DECR R2)
    
    (PUSH R1)
    (PUSH R2)
    (PUSH (:CONST 1))
    
    (JSR fact)
    (POP R1)
    (POP R1)
    (POP R1)
    (MUL R1 R0)
    
    (LABEL RTN_FACT)
    (RTN)