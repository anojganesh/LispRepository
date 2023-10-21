
(defun max-capacity (b n)
        (progn
                (defvar *g1*)
                (defvar *g2*)

                (if (= b 1)
                        (setf *g1* n)
                        (if (= b 2)
                                (setf *g2* n)))
        )
)

(defun enter-garage (b)
        (if (= b 1)
                        (setf *g1* (1- *g1*))
                        (if (= b 2)
                                (setf *g2* (1- *g2*))))
)

(defun exit-garage (b)
        (if (= b 1)
                        (setf *g1* (1+ *g1*))
                        (if (= b 2)
                                (setf *g2* (1+ *g2*))))
)
