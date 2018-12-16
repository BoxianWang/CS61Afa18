(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (if (null? rests) nil
    (cons (append (cons first nil) (car rests)) (cons-all first (cdr rests)))
  )
)

(define (zip pairs)
  'replace-this-line)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (helper l n)
    (if(null? l) nil
      (cons (list n (car l)) (helper (cdr l) (+ n 1)))
    )
  )
  (helper s 0)
  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond ((= total 0) '(()) )
        ((< total 0) nil)
        ((null? denoms) nil)
        (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
        (list-change total (cdr denoms))))
  )
  )
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (create-params lst)
  (if (null? lst) nil
    (cons (caar lst) (create-params (cdr lst)))
  )
)

(define (create-value lst)
  (if (null? lst) nil
    (cons (car (cdar lst)) (create-value (cdr lst)))
  )
)

(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (append (list form params) (let-to-lambda body))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (append (cons `(lambda ,(create-params values) ,(let-to-lambda (car body))) nil) (let-to-lambda (create-value values)))
           ; END PROBLEM 19
           ))
        (else   
         ; BEGIN PROBLEM 19
          (map let-to-lambda expr)
         ; END PROBLEM 19
         )))


        (cond ((null? expr) expr)
                ((null? (cdr expr)) expr)
            (else (cons (car expr) `(,(let-to-lambda (car(cdr expr))) ,(let-to-lambda (cadr (cdr expr))))))
           )