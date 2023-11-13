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

;;

(defun is-palindrome? (list)
	(cond 
		((= (length list) 1) t)
		((= (length list) 0) t)
		((eq (first list) (first (last list))) (is-palindrome? (reverse (rest (reverse (rest list))))))
		(t (= 1 0))
	)

)

(is-palindrome? '(1 2 3 3 2 1))

;;-- iterative solutions


;;--
(defvar book title author genre pages)

(defvar *arr*
(make-array '(6) :initial-contents
				(list (make-book :title "A" :author "Steve-O" :genre "Mystery" :pages 555)
				(make-book :title "C" :author "RICK" :genre "Mystery" :pages 444)
				(make-book :title "B" :author "PATRICK" :genre "HORROR" :pages 12)
				(make-book :title "G" :author "FRANK" :genre "CHILDRENS" :pages 800)
				(make-book :title "H" :author "JOE" :genre "ROMANCE" :pages 333)
				(make-book :title "E" :author "MANDY" :genre "CHILDRENS" :pages 501))
))

(defun max-books-pages (books-array)
	(let ((max-book (aref books-array 0)))
		(dotimes (i (length books-array))
			(let ((cb (aref books-array i)))
				(when (> (book-pages cb) (book-pages max-book))
					(setf max-book cb)
				)
			)
		)
		(progn
			max-book
		)
	)
)

(max-books-pages *arr*)

;;--

(defun last-books-author (books-array)
	(let ((last-book (aref books-array 0)))
		(dotimes (i (length books-array))
			(let ((cb (aref books-array i)))
				(write (string< (book-author cb) (book-author last-book))) ;; debugging
				(when (string> (book-author cb) (book-author last-book))
					(setf last-book cb)
				)
			)
		)
		(progn
			last-book
		)
	)
)

(last-books-author *arr*)

--

(defstruct book title author genre pages)

(defun max-books (books-array function)
	(dotimes i (length books-array))
)

;;--

(defstruct book
  title author genre pages)


(defun max-books (arr f) ;; f is a given function, likely a lambda function
  (let ((max-book (aref arr 0))) ;; set max-book to first element (assuming its valid)
    (dotimes (i (length arr) max-book) ;; iterate through the index values, return max
      (when (not (null (aref arr i)))
          (when (funcall f max-book (aref arr i)) ;; if function determines this is the new max
              (setf max-book (aref arr i))))))) ;; update the current max


 (max-books *arr* #'(lambda (x y) (< (book-pages x) (book-pages y))))
 
 (load "~/Documents/CPS305/buffertest.lisp" )
 
 ;;--
 
 (defun redact-names (list name)
 
 )
 
;; -- 
 
 (defun interleave (list1 list2 &optional (acc '()))
	(cond 
		((null list1) (push list2 acc))
		((null list2) (push list1 acc))
		(t (interleave (rest list1) (rest list2) (push (first list2) (push (first list1) acc))))
	)
 )
 
 ;;--

  (defun interleave (list1 list2 &optional (acc '()))
	(cond 
		((null list1) (reverse (append list2 acc)))
		((null list2) (reverse (apppend list1 acc)))
		(t (interleave (rest list1) (rest list2) (append (append (list (first list2)) (list (first list1))) acc)))
	)
 )
 
 (interleave '(a b c d e) '(1 2 3 4 5 6))

;;--

(defun mapf (function list &optional (acc '()))
	(cond 
		((and (null list) (eq acc '())) nil)
		((null list) (reverse acc))
		(t (mapf function (rest list) (push (funcall function (first list)) acc)))
	)
 )
 
 (mapf #'(lambda (x) (1- x)) '())
  
 (mapf #'(lambda (x) (1- x)) '(2 5 9 6))
