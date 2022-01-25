(defun fibonacci(n)
		(cond
			((eql n 1) 0)
			((eql n 2) 1)
			((eql T T) (+ (fibonacci (- n 1)) (fibonacci (- n 2))))
		)
	)

(write (fibonacci 10))
