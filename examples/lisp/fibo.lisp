(defun fibonacci(n)
	(cond
		((eql n 0) 0)
		((eql n 1) 1)
		((eql T T) (+ (fibonacci (- n 1)) (fibonacci (- n 2))))
	)
)

(write (fibonacci 10))
