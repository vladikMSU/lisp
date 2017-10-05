(load "kolesnikov.lisp")

(defun _cleanVar (var_type props_list)
  (cond
    ((null props_list))
    ((remprop var_type (car props_list)) (_cleanVar var_type (cddr props_list)))
    ((fatalError "cleanVar"))
  )
)

(defun cleanVar (var_type) 
  (_cleanVar var_type (SYMBOL-PLIST var_type))
)

(defun cleanVars ()
  (and
    (cleanVar 's) 
    (cleanVar 'v) 
    (cleanVar 'w) 
    (cleanVar 'e)
  )
)

(defun printValsOfType (var_type props_list)
  (cond ((null props_list))
        ((and (print (concatAll var_type (car props_list) " => " (cadr props_list)))
              (printValsOfType var_type (cddr props_list))
        ))
  )
)

(defun printVals () 
  (and (printValsOfType 's (SYMBOL-PLIST 's))
       (printValsOfType 'w (SYMBOL-PLIST 'w))
       (printValsOfType 'e (SYMBOL-PLIST 'e))
       (printValsOfType 'v (SYMBOL-PLIST 'v))
  )
)

(defun startTesting (lst)
  (cond ((null lst))
        ((and 
            (print (concatenate 'string "----------TEMPLATE:" (toString (car lst))))
            (print (concatenate 'string "----------LIST:    " (toString (cadr lst))))
            (or (Match (car lst) (cadr lst)) t)
            (startTesting (cddr lst))
        ))
  )
)

(startTesting 
  '(
    ((e 1) (s a) (e 1) (s a))       (a b)

    ((w 1) (w 1))                   ((m (b)) (m (b)))

    ((e 1) (e 1) (e 1) (s a))       (a a a a)

    ((e 1) (e 1) (e 1) (s a))       (a a a a a)

    ((e 1) (e 1) (e 1) (s a))       (a a a a a a)

    ((e 1) (e 1) (e 1) (s a))       (a a a a a a a)

    ((e 1) (e 1) (e 2) (e 1) (e 1)) (a a a a)
      
    ((e 1) (e 2) (e 2) (e 1))       (a a) 
    
    ((e 1) (e 2) (e 1))             (a a)

    ((e 1) ((e 2) (e 3)))           (a (b c))

    (a b c)             (a b)
  )
)
