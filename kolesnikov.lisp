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

(defun fatalError (msg_to_log)
  (print (concatenate 'string "FATAL ERROR: " msg_to_log))
)

; some other useful functions

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

(defun ne (x y) 
  (not (eq x y))
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

(defun takeLstTail (n lst)
  (cond 
    ((null lst) nil)
    ((= n 0) lst)
    ((takeLstTail (- n 1) (cdr lst)))
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
        ((or (null lst) (not (atom (car lst)))) nil)

        ; if refal_var_value exists then:
        ; -- if previous s's value equals to lst's first element
        ;    continue matching with lst's tail
        ; -- return nil else
        (refal_var_value
         (cond ((ne refal_var_value (car lst)) nil)
               ((Match template (cdr lst)))
         )
        )

        ; refal_var_value doesn't exist here and lst's first element is atom, 
        ; remember it's value and continue
        ((assignMatchRemove refal_var template lst))
      )
  )
)

(defun wMatchRefalTemplate (refal_var template lst)
  (let ((refal_var_value (get (car refal_var) (cadr refal_var))))
      (cond
        ; if refal_var_value exists then:
        ; -- if previous w's value equals to lst's first element 
        ;    continue matching with lst's tail
        ; -- return nil else
        (refal_var_value
         (cond ((smartNe refal_var_value (car lst)) nil)
               ((Match template (cdr lst)))
         )
        )

        ; refal_var_value doesn't exist here
        ; remember it's value and continue
        ((assignMatchRemove refal_var template lst))
      )
  )
)

(defun assignRefalVar (refal_var value)
  (cond ((null value) (setPropFor (car refal_var) (cadr refal_var) '\nil))
        ((setPropFor (car refal_var) (cadr refal_var) value))
  )
)

(defun elementNumberPrediction (n refal_var template lst)
  (let ((firstNElems (takeFirstNElements n lst)))
    (cond
      ((not (= n (smartLen firstNElems))) nil)
      ((get (car refal_var) (cadr refal_var))
       (or (Match template (takeLstTail n lst))
           (elementNumberPrediction (+ n 1) refal_var template lst))) 
      ((or 
         (and (assignRefalVar refal_var firstNElems) nil)
         (Match template (takeLstTail n lst))
         (and (remprop (car refal_var) (cadr refal_var)) nil)
         (elementNumberPrediction (+ n 1) refal_var template lst)
      ))
    )
  )
)

(defun evMatchRefalTemplate (n refal_var template lst) 
  (let ((refal_var_value (get (car refal_var) (cadr refal_var))))
    (cond
      ; if refal_var_value exists
      (refal_var_value
       (let ((list_len (smartLen refal_var_value)) (firstListLenElems (takeFirstNElements (smartLen refal_var_value) lst)))
         (cond
           ((not (= list_len (smartLen firstListLenElems))) nil)
           ((smartEq refal_var_value firstListLenElems) (Match template (takeLstTail (smartLen firstListLenElems) lst)))
         )
       )
      )
      ((elementNumberPrediction n refal_var template lst))
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
    ((or (null template)
         (atom (car template))
         (ne (smartLen (car template)) 2)))

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

    ; process refal variables
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
