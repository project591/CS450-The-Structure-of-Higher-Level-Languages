;#1
; first part inlined - works.
(define make-account-lambda
  (lambda (balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define deposit
    (lambda (amount)    
    (set! balance (+ balance amount))
    balance))
    (lambda (m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))))

;(display "testingn make account lambda")
;(newline)
;(define acc (make-account-lambda 100))
;((acc 'withdraw) 50)

; second part -  make-account-inline
(define make-account-inline
  (lambda (balance)
    (lambda (m)
      (cond ((eq? m 'withdraw)
             (lambda (amount)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount)) balance)
                   "Insufficient funds")))
            ((eq? m 'deposit)
             (lambda (amount)
               (set! balance (+ balance amount))
               balance))
            (else (error "Unknown request -- MAKE-ACCOUNT " m))))))

;(display "testing make account inline")
;(newline)
;(define acc-inline (make-account-inline 100))
;((acc-inline 'withdraw) 50)
;(newline)

; make-account-inline-factored - Extra Credit
(define make-account-inline-factored
  (lambda (balance)
    (lambda (m)
      (lambda (amount)
        (cond ((eq? m 'withdraw) 
               (if (>= balance amount)
                   (begin (set! balance (- balance amount))
                          balance)
                   "Insufficient funds"))
              ((eq? m 'deposit) 
               (set! balance (+ balance amount))
               balance)
              (else (error "Unknown request --> MAKE-ACCOUNT" m)))))))

;(display "testing make account inline")
;(newline)
;(define acc-fac (make-account-inline-factored 100))
;((acc-fac 'withdraw) 50)
;(newline)


; #3
(define (square x) (* x x)) ; square function

; monitor function. 
(define (make-monitored f)
  (let ((counter 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) counter)
            ((eq? x 'reset-count) (set! counter 0) counter)
            (else (set! counter (+ 1 counter))
                  (f x))))))

;(define s (make-monitored square))
;(display "testing make monitor: ")
;(display (s 86))
;(newline)
;(s 'how-many-calls?)
;(newline)

; #4 - making this password protected.
; im using the first changed version of
; the first probelm
(define make-pw-account
  (lambda (balance password) ; will pass a password here. 
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define deposit
    (lambda (amount)    
    (set! balance (+ balance amount))
    balance))
    (lambda (p m)
      ; Below I test to see if the password is correct. 
    (cond ((not (equal? p password)) (lambda (x) "Wrong Password")) 
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))))


;(define account (make-pw-account 100 'alex))
;((account 'alex 'withdraw) 20)
;((account 'lambchops 'withdraw) 100)