(define (cddr s)
  (cdr (cdr s)))

(define (cadr s)
  (car (cdr s))
)

(define (caddr s)
  (car (cdr (cdr s)))
)

(define (sign x)
  (cond
    ((< x 0) -1)
    ((= x 0) 0)
    ((> x 0) 1)
  )
)

(define (square x) (* x x))

(define (pow b n)

  (if (= n 0) 1 (if (even? n) (square (pow b (/ n 2))) (* b (square (pow b (/ (- n 1) 2))))))
)

(define (ordered? s)
  (if (empty? s) 
    #t
    (if (not(empty? (cdr s)))
      (if (<= (car s) (car (cdr s))) 
        (ordered? (cdr s))
        #f
      )
      #t
    )
  )
)

(define (empty? s) (null? s))

(define (add s v)
  (cond ((empty? s) (list v))
    ((= v (car s)) s)
    ((< v (car s)) (cons v s))
    (else (cons (car s) (add (cdr s) v)))
  )
)

; Sets as sorted lists
(define (contains? s v)
  (cond ((empty? s) #f)
    ((< v (car s)) #f)
    ((= v (car s)) #t)
    (else (contains? (cdr s) v))
  )
)

; Equivalent Python code, for your reference:
;
; def empty(s):
;     return s is Link.empty
;
; def contains(s, v):
;     if empty(s):
;         return False
;     elif s.first > v:
;         return False
;     elif s.first == v:
;         return True
;     else:
;         return contains(s.rest, v)

(define (intersect s t)
  (cond ((or (empty? t) (empty? s)) nil)
    ((= (car s) (car t)) (cons (car s) (intersect (cdr s) (cdr t))))
    ((> (car t) (car s)) (intersect (cdr s) t))
    (else (intersect s (cdr t)))
    )
)

; Equivalent Python code, for your reference:
;
; def intersect(set1, set2):
;     if empty(set1) or empty(set2):
;         return Link.empty
;     else:
;         e1, e2 = set1.first, set2.first
;         if e1 == e2:
;             return Link(e1, intersect(set1.rest, set2.rest))
;         elif e1 < e2:
;             return intersect(set1.rest, set2)
;         elif e2 < e1:
;             return intersect(set1, set2.rest)

(define (union s t)
  (cond ((empty? s) t)
    ((empty? t) s)
    ((contains? s (car t)) (union s (cdr t)))
    (else (union (add s (car t)) (cdr t)))
    )
)