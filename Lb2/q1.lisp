(defun grade (p)
        (cond   ((and (<= p 100) (>= p 90)) 'A+)
                        ((and (<= p 89) (>= p 85)) 'A)
                        ((and (<= p 84) (>= p 80)) 'A-)
                        ((and (<= p 79) (>= p 77)) 'B+)
                        ((and (<= p 76) (>= p 73)) 'B)
                        ((and (<= p 72) (>= p 70)) 'B-)
                        ((and (<= p 69) (>= p 0)) 'F)


        )
)
