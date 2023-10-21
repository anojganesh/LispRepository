;;; Recursive fact function

(defun fact (n)
  (if (> n 1)
      (* n (fact (- n 1)))
          1))



