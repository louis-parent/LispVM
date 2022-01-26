(defun set_variable ()
    (setf (get 'name 'alphabet) '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
    (setf (get 'name 'value) 7)
)

(defun reverse_alphabet ()
    (let
        (
            (current (get 'name 'alphabet))
        )
        (reverse_list current)
    )
)

(defun reverse_list (l)
    (if (null l)
        '()
        (append (reverse_list (cdr l)) (list (car l)) )
    )
)

(defun all_test ()
    (progn
        (set_variable)
        (write (reverse_alphabet))
        (write (- 7 2))
    )
)

(all_test)

