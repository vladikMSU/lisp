(load "kolesnikov.lisp")

; helping functions

(defun toString (item)
  (princ-to-string item) 
)

(defun _concatAll (lst) 
  (cond
    ((null lst) "")
    ((atom (car lst)) 
     (concatenate 'string (toString (car lst)) " " (_concatAll (cdr lst))))
    ((concatenate 'string "(" (_concatAll (car lst)) ") " (_concatAll (cdr lst))))
  )
)

(defun concatAll (&rest lst)
  (_concatAll lst)
)

; printing result

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

; cleaning stack

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

; testing

(defun testAll (lst)
  (cond ((null lst)) 
    ((and (print (concatenate 'string "----------TEMPLATE:" (toString (car lst))))
          (print (concatenate 'string "----------LIST:    " (toString (cadr lst))))
          (print (concatenate 'string "RESULT: " (toString (Match (car lst) (cadr lst)))))
          (printVals)
          (cleanVars)
          (testAll (cddr lst))
    ))
  )
)

(testAll
  '(
    ((e 1) (s a) (e 1) (s a))       (a b)

    ((w 1) (w 1))                   ((m (b)) (m (b)))

    ((w 1) (w 2))                   ((m (b)) (x y))

    ((e 1) (e 1) (e 1) (s a))       (a a a a)

    ((e 1) (e 1) (e 1) (s a))       (a a a a a)

    ((e 1) (e 1) (e 1) (s a))       (a a a a a a)

    ((e 1) (e 1) (e 1) (s a))       (a a a a a a a)

    ((e 1) (e 1) (e 2) (e 1) (e 1)) (a a a a)
      
    ((e 1) (e 2) (e 2) (e 1))       (a a) 
    
    ((e 1) (e 2) (e 1))             (a a)

    ((e 1) ((e 2) (e 3)))           (a (b c))

    ((e 1) ((v 2) (v 3)))           (a (b c))

    (a b c)                         (a b)

    (a b (w 1))                     (a b)
  )
)
