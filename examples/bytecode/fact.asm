(JMP main)

(LABEL fact)
    
(MOVE FP R0)
(MOVE (:CONST -1) R1)
(ADD R0 R1)
(LOAD R1 R1);R1 contient le parametre

(MOVE (:CONST 1) R0)

(CMP R1 (:CONST 1))
(JEQ RTN_FACT)

(MOVE R1 R2)
(DECR R2)

(PUSH R1)
(PUSH R2)
(PUSH (:CONST 1))

(JSR fact)
(POP R1);depiler const 1
(POP R1);depiler R2
(POP R1);depiler R1
(MUL R1 R0)

(LABEL RTN_FACT)
(RTN)

(LABEL main)
    
(PUSH (:CONST 5)); le paramètre
(PUSH (:CONST 1)); le nombre de paramètre
(JSR fact)

(HALT)