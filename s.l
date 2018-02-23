(
  defun isLL2( q )
  (

    cond
        ( (numberp q)  nil )
        ( (= (length q)  2)  t)
        (t nil)
  )
)

(
  defun isLL2a(q)
  (
    cond
        ( (numberp q)  nil )
        (
          (and
            (= (length q)  2)  (listp (cadr q))
          )
        )
        (t nil)
  )
)

(
  defun isLpairs-recursive (q)
  (cond
      ( (null q) t )
      ( (numberp q)  nil )
      ( (isLL2 (car q) ) (isLpairs-recursive ( cdr q )) )
      ( t nil )
  )
)


(defun isLpairs-iterative (q)
  (cond
    ((null q) t)
    ((numberp q) nil)
    (t (do
        (
          (q1 q(cdr q1))
        )
        ((null q1) res)
        (setq res (isLL2(car q1)))
        (cond
          ((eql res nil) (return nil) )
        )
      )
    )
  )
)

(defun isLpairs-every (q)
  (cond
      ( (null q) t )
      ( (numberp q)  nil )
      ( t (every #'isLL2 q) )
  )
)

(defun isLpairs1st-recursive (q)
    (cond
        ( (null q) t )
        ( (numberp q)  nil )
        ( (isLL2 (car q) ) (isLpairs1st-recursive_helper q) )
        ( (isLL2 (car q) ) (isLpairs1st-recursive ( cdr q )))
        ( t nil )
    )
)

(defun isLpairs1st-recursive_helper (q)
    (cond
        ( (null (cadr q)) t )
        ( (numberp (cadr q))  nil )
        ( (equal (car(car q)) (car(cadr q)) ) t )
        ( t nil )
    )
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



; ________________ part 3


(defun distribute-helper-recursive (q p)
  (cond
    ( (null p) nil)
    ( t (cons
              (list q (car p) )
              (distribute-helper-recursive q (cdr p) )
        )
    )
  )
)

(defun distribute-recursive (q)
  (cond
    ( (null q) nil )
    ( (not (isLL2a q) )  nil)
    ( t (distribute-helper-recursive (car q) (cadr q) ))
  )
)

(defun distribute-iterative (q)
  (cond
    ( (null q) nil )
    ( (not (isLL2a q) )  nil)
    (t
      (do
        (
          (q1 (cadr q)(cdr q1))
          (res nil)
        )
        ((null q1) (return res))
        (setq res
                  (append
                          res (list(list (car q) (car q1)))
                  )
        )

      )
  )
  )
)

(defun distribute-mapcar (q)
      (cond
        ( (null q) nil )
        ( (not (isLL2a q) )  nil)
        ( t
            (mapcar #'list (distribute-mapcar-helper (car q ) (cadr q) ) (cadr q) )
        )
      )

)

(defun distribute-mapcar-helper (q r)
        (do
          (
            (i 0 (+ 1 i))
            (res nil)
          )
          ( (eql i (length r ) ) res )
          (setq res
                    (append
                            res (list q)
                    )
          )

        )
)


; ________________
(defun undistribute-recursive-helper (q)
    (cond
      ( (null q) nil)
      ( t
        (cons
              (cadr (car q)) (undistribute-recursive-helper (cdr q) )
        )

      )
    )
)

(defun undistribute-recursive (q)
      (cond
        ( (null q) nil )
        ( (eql (isLpairs1st-recursive q) nil) nil )
        ( t
          (list (caar q) (undistribute-recursive-helper q ) )
        )
      )

)


(defun undistribute-iterative (q)
  (cond
    ( (null q) nil )
    ( (eql (isLpairs1st-recursive q) nil) nil )
    ( t
        (do
          (
            (q1 q (cdr q1))
            (res nil)
          )
          ((null q1) (return (list (caar q)res)))
          (setq res
                    (append
                            res (list (cadr (car q1)))
                    )
          )

        )
    )
  )
)


(defun undistribute-mapcar (q)
  (cond
    ( (null q) nil )
    ( (eql (isLpairs1st-recursive q) nil) nil )
    ( t
          (cons
              (caar q)
              (list(mapcar #'cadr q ))
          )
    )
  )
)


; ________________


(defun my-every (fun q)
  (cond
        ( (null q) t )
        ( t
                (and
                  (funcall fun (car q))
                  (my-every fun (cdr q))
                )
        )

  )
)

; ________________ ________________ ________________ ________________ ________________ ________________ ________________ ________________
(defun lenLL-helper (q)
  (cond
        ((null q) 0 )
        ((listp (car q))
          (max (length q) (length (car q)) (lenLL-helper(car q))(lenLL-helper(cdr q)))
        )
        (t (max (length q) (lenLL-helper(cdr q))))
  )
)

(defun lenLL (q)
  (cond
        ((null q) 0   )
        ((eql (listp q) nil) 0   )
        (t
          (lenLL-helper q)
        )
  )
)
