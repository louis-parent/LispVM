(MOVE (:CONST 5) R1)
(MOVE (:CONST 1) R2)

(LABEL fact)

(CMP R1 (:CONST 1))
(JLE ret)
(MUL R1 R2)
(DECR R1)
(JMP fact)

(LABEL ret)
(MOVE R1 R0)