(defstruct person name age)



(defun filter-ages (array threshold)
	(let ((vec (make-array 0 :adjustable t :fill-pointer t))) 
	
	(dotimes (i (length array) vec)
		(when (> (person-age (aref array i)) threshold)
			(vector-push-extend (person-name (aref array i)) vec))
	)
	)
)

(defun array-group (array1 array2)
	(let ((vec (make-array 0 :adjustable t :fill-pointer t)))
	(dotimes (i (max (length array1) (length array2)) vec)
			(let((tuple (make-array 2)))
				(if (< i (length array1))
					(setf (aref tuple 0) (aref array1 i))
					(setf (aref tuple 0) nil)
				)
				(if (< i (length array2))
					(setf (aref tuple 1) (aref array2 i))
					(setf (aref tuple 1) nil)
				)
				(print tuple)
				(vector-push-extend tuple vec))
		)
		
	)
)


;;; tests

(let ((v (vector
               (make-person :name "Alice" :age 25)
               (make-person :name "Bob" :age 30)
               (make-person :name "Charlie" :age 20)
               (make-person :name "David" :age 35))))
         (filter-ages v 25))

(array-group #(1 3 5) #(2 4 6 8 10))
