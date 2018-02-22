(defun isLL2 (q) ;checks if list is of exactly size 2
	(cond
		;((null q) nil)
		;((numberp q) nil)
		((listp q)(= (length q) 2))	
		
		(t nil)
	)
)

(defun isLL2a (q) ;checks if list is exactly of length 2 and the 2nd element of a list is a list
	
	(cond
		( (isLL2 q)(listp (cadr q)) )
		( t nil )
	)
)

(defun isLpairs-recursive (q)	;recursively checks if list elements are of length 2 ie- pairs
	(cond
		((null q) t)
		((numberp q ) nil)
		( (isLL2(car q)) (isLpairs-recursive(cdr q)) )
		( t nil )
	)
)

(defun isLpairs-iterative (q)	;iteratively checks if list elements are pairs
	(cond 
		((null q) t)
		((numberp q) nil)
		
		(t (do
				((q1 q (cdr q1))
				 (res t (setq res (isLL2(car q1))))

				)

				((null q1) res)
				(cond
					((eq res t))
					(t (return nil))
				)	
			)
		)	
	)
)

(defun isLpairs-every (q)	; uses every to go elements by element in a list and check if it is a pair (length of 2)
	(cond
		((null q) t)
		((numberp q) nil)
		( t(every #'isLL2 q))
	)
	
)

(defun isLpairs1st-recursive (q)	;checks if List is of pairs and that each pair has same element object, type, and value
	
		(and (isLpairs-recursive q) (isLpairs-helper (caar q) (cdr q)))	;calls helper function where recursion occurs
	 
)

(defun isLpairs-helper (q r)	;here the helper checks if the list is nil, then checks if the element in the first list is equal to first element of the cdr
	(cond
		((null r) t)

		((equal q (caar r)) (isLpairs-helper q (cdr r)))
		
		(t nil)
	)
)

(defun isLpairs1st-iterative (q) 	;function is done iteratively, calls iterative funciton already created and checks
	(and (isLpairs-iterative q) (isLpairs-helper (caar q) (cdr q)))
)

(defun isLpairs1st-every (q) 	;function is done using every function created above.
	(and (isLpairs-every q) (isLpairs-helper (caar q) (cdr q)))
)

(defun distribute-recursive (q)
	(cond
		((eq (isLL2a q) nil) nil)
		(t (distributive-helper (car q) (cadr q)))
	)
)

(defun distributive-helper (p q)
	(cond
		((null q) nil)
		;((atom q) (list p q))

		(t (cons 
			(list p (car q))
			(distributive-helper p (cdr q))
		    )
		)
	)
)

(defun distribute-iterative (q)
	(cond
		((eq (isLL2a q) nil) nil)
		
		(t(do(
				(q1 (car q))
				(q2 (cadr q) (cdr q2))
				(res nil (append res (list(list q1 (car q2)))))
			)
				((null q2) res)
			)
		)
	)
)

(defun distribute-mapcar (q)
	(cond
		((eq (isLL2a q) nil) nil)
		(t
			(append (mapcar #'list '(1 1 1) '(a b c)))
		)
	)
)

