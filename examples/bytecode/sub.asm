(MOVE (:CONST 1) R1)
(MOVE (:CONST 2 ) R2)
(PUSH R1)
(PUSH R2)
(MOVE (:CONST 2 ) R2)
(PUSH R2)
(JSR add)
(HALT)

(LABEL add)
(MOVE FP R1)
(MOVE (:CONST 1) R2)
(SUB R1 R2)
(LOAD R2 R0)

(MOVE FP R1)
(MOVE (:CONST 2) R2)
(SUB R1 R2)
(LOAD R2 R1)
(SUB R1 R0)
(RTN)
