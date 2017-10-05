; Колесников Владислав, 424 (бывшая 324) группа.
; Отождествление рефал выражений.
;
; Входные данные поступают в виде двух списков:
; -- список-шаблон, элементами которого являются:
;    1) произвольные атомы;
;    2) двухэлементные списки - пары атомов (<рефал_тип> <имя_переменной>),
;       где <рефал_тип> это: S - атом;
;                            W - лисп-выражение (атом или список);
;                            E - произвольная последовательность лисп-выражений;
;                            V - непустая последовательность лисп-выражений;
;    3) стуктурные скобки;
; -- произвольный список, с которым производится попытка
;    отождествления по правилам языка Рефал.
;
; Тестирование производится отдельной программой.


; assign/debug functions

(defun setPropFor (name prop val)
    (setf (get name prop) val )
)

(defun fatalError (msg)
  (print (concatenate 'string "FATAL ERROR: " msg))
)

; some other useful functions

(defun ne (x y) 
  (not (eq x y))
)

(defun smartEq (lst1 lst2)
  (cond
    ((or (and (eq lst1 '\nil) (null lst2)) 
         (and (eq lst2 '\nil) (null lst1)))
    )
    ((and (null lst1) (null lst2)) t)
    ((or (null lst1) (null lst2)) nil)
    ((atom lst1) (eq lst1 lst2))
    ((and (smartEq (car lst1) (car lst2)) (smartEq (cdr lst1) (cdr lst2))))
  )
)

(defun smartNe (lst1 lst2)
  (not (smartEq lst1 lst2))
)

(defun smartLen (lst)
  (cond ((eq lst '\nil) 0)
        ((length lst))
  )
)

(defun smartMember (memb lst)
  (cond ((null lst) nil)
        ((or (smartEq memb (car lst)) (smartMember memb (cdr lst))))
  )
)

(defun takeFirstNElements (n lst)
  (cond 
    ((null lst) nil)
    ((= n 0) nil)
    ((cons (car lst) (takeFirstNElements (- n 1) (cdr lst))))
  )
)

(defun dropFirstNElements (n lst)
  (cond 
    ((null lst) nil)
    ((= n 0) lst)
    ((dropFirstNElements (- n 1) (cdr lst)))
  )
)

(defun isRefalVariable (lst)
  (cond
    ((atom lst) nil)
    ((ne (smartLen lst) 2) nil)
    ((or  (eq (car lst) 'e)
          (eq (car lst) 's)
          (eq (car lst) 'w)
          (eq (car lst) 'v)
    ))

  )
)

; functions for matching refal templates

(defun assignMatchRemove (refal_var template lst) 
  (or
    (and (setPropFor (car refal_var) (cadr refal_var) (car lst)) nil)
    (Match template (cdr lst))
    (and (remprop (car refal_var) (cadr refal_var)) nil)
  )
)

(defun sMatchRefalTemplate (refal_var template lst) 
  (let ((refal_var_value (get (car refal_var) (cadr refal_var))))
      (cond
        ; if lst is empty or its first element is not atom - matching fails
        ((or (null lst) (not (atom (car lst)))) nil)

        ; if variable of this type with such name has been assigned then:
        ; -- if its value equals to lst's first element
        ;    continue matching
        ; -- else matching fails. return nil
        (refal_var_value
         (cond ((ne refal_var_value (car lst)) nil)
               ((Match template (cdr lst)))
         )
        )

        ; this variable hasn't been assigned, 
        ; assign it's value, continue matching and remove value if matching fails
        ((assignMatchRemove refal_var template lst))
      )
  )
)

(defun wMatchRefalTemplate (refal_var template lst)
  (let ((refal_var_value (get (car refal_var) (cadr refal_var))))
      (cond
        ; if lst is empty - matching fails
        ((null lst) nil)

        ; if variable of this type with such name has been assigned then:
        ; -- if its value equals to lst's first element 
        ;    continue matching
        ; -- else matching fails. return nil
        (refal_var_value
         (cond ((smartNe refal_var_value (car lst)) nil)
               ((Match template (cdr lst)))
         )
        )

        ; this variable hasn't been assigned
        ; assign it's value, continue matching and remove value if matching fails
        ((assignMatchRemove refal_var template lst))
      )
  )
)

(defun assignRefalVar (refal_var value)
  (cond ((null value) (setPropFor (car refal_var) (cadr refal_var) '\nil))
        ((setPropFor (car refal_var) (cadr refal_var) value))
  )
)

(defun predictElementsNumber (n refal_var template lst)
  (let ((first_n_elems (takeFirstNElements n lst)))
    (cond ((not (= n (smartLen first_n_elems))) nil)

          ; see if matching succeeds after assigning 
          ; first n elements to current variable
          ; if it doesn't, try with n+1 
          ((or (and (assignRefalVar refal_var first_n_elems) nil)
               (Match template (dropFirstNElements n lst))
               (and (remprop (car refal_var) (cadr refal_var)) nil)
               (predictElementsNumber (+ n 1) refal_var template lst)))
    )
  )
)

(defun evMatchRefalTemplate (n refal_var template lst) 
  (let ((refal_var_value (get (car refal_var) (cadr refal_var))))
    (cond
      ; if such variable has already been assigned:
      ; -- if there are enough elements left in lst
      ;    compare them with variable value and continue matching
      ; -- else matching fails. return nil
      (refal_var_value
       (let ((value_len       (smartLen refal_var_value))
             (first_LEN_elems (takeFirstNElements (smartLen refal_var_value) lst)))
          (cond ((not (= value_len (smartLen first_LEN_elems))) nil)
                ((smartEq refal_var_value first_LEN_elems)
                 (Match template (dropFirstNElements (smartLen first_LEN_elems) lst)))
          )
       )
      )

      ; such variable met for the first time
      ; predict its value
      ((predictElementsNumber n refal_var template lst))
    )
  )
)

(defun eMatchRefalTemplate (refal_var template lst) 
  (evMatchRefalTemplate 0 refal_var template lst)
)

(defun vMatchRefalTemplate (refal_var template lst) 
  (evMatchRefalTemplate 1 refal_var template lst)
)

(defun matchRefalTemplate (template lst)
  (cond
    ((eq (car (car template)) 's) (sMatchRefalTemplate (car template) (cdr template) lst))
    ((eq (car (car template)) 'w) (wMatchRefalTemplate (car template) (cdr template) lst))
    ((eq (car (car template)) 'e) (eMatchRefalTemplate (car template) (cdr template) lst))
    ((eq (car (car template)) 'v) (vMatchRefalTemplate (car template) (cdr template) lst))
    ((fatalError "matchRefalTemplate"))
  )
)

; main function

(defun Match (template lst)
  (cond
    ; template and lst are empty. end of parsing
    ((and (null template) (null lst)) T)

    ; template is empty and lst is not. matching fails
    ((null template) nil)

    ; first element in template and first element in lst
    ; are atoms and they are not equal. matching fails
    ((and (atom (car template))
          (ne (car template) (car lst)))
     nil)

    ; first elements are equal atoms. recursive continue
    ((and (atom (car template)) 
          (eq (car template) (car lst))) 
     (Match (cdr template) (cdr lst)))

    ; match refal variables and the rest of template
    ((isRefalVariable (car template)) (matchRefalTemplate template lst))

    ; first element of template is list and
    ; first element in lst is atom. matching fails
    ((and (not (atom (car template)))
          (atom (car lst)))
     nil)
        
    ; template's and list's first elements are lists.
    ; recursive continue
    ((and (Match (car template) (car lst))
          (Match (cdr template) (cdr lst))))
  )
)
