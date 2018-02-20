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

(defun isLpairs-recursive (q)
	(cond
		((null q) t)
		((numberp q ) nil)
		( (isLL2(car q)) (isLpairs-recursive(cdr q)) )
		( t nil )
	)
)

(defun isLpairs-iterative (q)
	;(cond
	;	((null q) t)
	;	((numberp q) nil)
	;	(t (do
	;			(
	;				(q1 q (cdr q1))
	;				(res t)
;
;				)
;				((eq res nil) nil)
;				((null q1) res)
;				(setq res (isLL2(car q1)))
;				
;			)
;		)
;	)
	(cond 
		((null q) t)
		((numberp q) nil)
		(t (do
				(
					(q1 q(cdr q1))
					(res nil (setq res (isLL2(car q1))))
				) 
				((eq res nil)nil)
				((null q1) res)
			)
			
		)
	)
	
)
(defun isLpairs-every (q)

)

