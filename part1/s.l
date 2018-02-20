(defun isLL2 (q) ;checks if list is of exactly size 2
	(cond
		((numberp q) nil)
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