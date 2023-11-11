;;Iterative

(defun merge-sorted-lists (list1 list2 compare)
	(let (res '())
		(do () ((and (null list1) (null list2))) 		;; while loop
			(let ((e1 (first list1))
				(e2 (first list2)))
				(cond 
					((null e1) (dolist (i list2) (push i res)) (return res))
					((null e2) (dolist (i list1) (push i res)) (return res))
					((funcall compare e1 e2) (push e1 res) (setf list1 (rest list1)))
					(t (push e2 res) (setf list2 (rest list2)))
					
				)
			)
		)
		(reverse res)
	)

)
(merge-sorted-lists '(1 2 3 4 5) '(3 5 6 7 8) '>)

;;Recursive Solution 

(defun merge-sorted-lists (list1 list2 compare &optional (acc '()))
	(let ((e1 (first list1)) (e2 (first list2)))
		(cond ((and (null list1) (null list2)) (reverse acc))
				((null list1) (reverse (append (reverse list2) acc)))
				((null list2) (reverse (append (reverse list1) acc)))
				
				((funcall compare e1 e2) (merge-sorted-lists (rest list1) list2 compare (cons e1 acc)))
				(t (merge-sorted-lists list1 (rest list2) compare (cons e2 acc)))	
				
		
		)
	)
	
)

(merge-sorted-lists '(1 2 3 4 5) '(3 5 6 7 8) '>)

(trace (merge-sorted-lists '(1 2 3 4 5) '(3 5 6 7 8) '<))

;;

(defun sum-deep-list-int (list &optional (acc 0))
	(cond 
		((null list) acc) 
		((typep (first list) 'integer) (sum-deep-list (rest list) (+ acc (first list))))
		((not (typep (first list) 'list)) (sum-deep-list (rest list) acc))
		(t (sum-deep-list (rest list) (+ acc (sum-deep-list(first list)))))
	)
)


(sum-deep-list-int '(1 2 "hello" 3 4 4.0 (5 6) 7 (8 'hi 9 (10 11 (12) "bye" (13 14)) 15 (16)) 17 'goodbye ((((18))) 19) 20))

;;

(defun flatten-list (list &optional (acc '()))

	(cond 
		((null list) acc)
		((listp (first list)) (flatten-list (rest list) (flatten-list (first list) acc)))
		(t (flatten-list (rest list) (push (first list) acc)))
	)
	)


(reverse (flatten-list '(1 2 3 4 (5 6) 7 (8 9 (10 11 (12) (13 14)) 15 (16)) 17 ((((18))) 19) 20)))
