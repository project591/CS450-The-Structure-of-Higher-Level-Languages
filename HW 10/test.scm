; found in section 2.2 of SICP textbook. 
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; ok so this somewhat works
; now how could this be used
; in implementing the let in the
; compiler.
; the 0 serves as a base case.
; This of recursion. 
(accumulate max 0 '(128 32 134 136))
(newline)
; the 0 serves like a base case.
; think of recursion. 
(accumulate + 0 '(1 2 3 4 5 6 7 8 9 10))


(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; almost make a game of word scramble. 
(define (let->combination expr) 
  (cons (make-lambda (let-vars expr) (let-body expr)) 
        (let-inits expr)))

; getting the required expression to make lambda
; from the let expression below. Very useful information. 


'(let ((x 34)) (list x))
(newline)
(display "testing below")
(newline)
; if it does then this part is almost too easy.
; could you apply a list to to the expression? more unique!
; would get second element with no recursion
;(define (let-vars expr) (list(caar (cadr expr))))
; get first element without recursion
;(define (let-vars expr) (list(caar (cadr expr)))) 
(define (let-vars expr) (map car (cadr expr))) ; this works
(define (let-inits expr) (map cadr (cadr expr))) 
(define (let-body expr) (cddr expr)) ; This part is unchangeable I think.

#|
(let-vars '(let ((x 34)) (list x)))
(newline)
(let-body  '(let ((x 34)) (list x)))
(newline)
(let-inits '(let ((x 34)) (list x)))
(newline)
(display "final result: ")

(let->combination '(let ((x 34)) (list x))) ; works!
|#
; could use below for testing for let with
; multiple variables.
; https://stackoverflow.com/questions/2906580/using-lambda-instead-of-let-in-scheme
  (define (lets x)
      (let ((a 10)
            (b 20))
        (+ x a b)))

;(lets 10)
(display "NOW FOR MULTIPLE VARIABLE LET")
(newline)

'(let ((a 10) (b 20)) (+ x a b))
(newline)
(let-vars '(let ((a 10) (b 20)) (+ x a b)))
(newline)
(let-body '(let ((a 10) (b 20)) (+ x a b)))
(newline)
(let-inits '(let ((a 10) (b 20)) (+ x a b)))

(newline)
;(cons 'a (cons 'b '()))


; ok this would return number change outer cadr to car
; to get the 10. Change back to cadr to get 20.
(cadr (cadr(cadr '(let ((a 10) (b 20)) (+ a b)))))
; returns ((b 10)) from ((a 10) (b 20))
'(let ((a 10) (b 20)) (+ x a b))
(display "working on the variables now")
(newline)
(caar (cdr '((a 10) (b 20))))
; ok trying to get the numbers now. 

(define (get-variables exp)
  (if (null? exp)
      '()
      (cons (caar exp) (get-variables (cdr exp)))))

(display "it works!") (newline)
(get-variables '((a 10) (b 20)))
(get-variables '((x 34)))
; below, show the let will be passed, with cadr, in
; the compile.scm
(get-variables (cadr'(let ((a 10) (b 20)) (+ x a b))))
(display "Now working on the numbers.") (newline)
(cadr (cadr '((a 10) (b 20))))

; got it to work.
(define (get-bindings exp)
  (if (null? exp)
      '()
      (cons (cadr (car exp)) (get-bindings (cdr exp)))))

(get-bindings (cadr'(let ((a 10) (b 20)) (+ x a b))))

;;;;;;;;; SOLUTION TO LET BELOW ;;;;;;;;;;

; In compiler.scm, in the dispatch, add this code and name
; it the way the prof. says so. Right now it is called
; (let->combination ........)

((let? exp) (compile (let->appl exp) target linkage))


; In syntax.scm add these.

; checking for the tag
(define (let? exp) (tagged-list? exp 'let)) 

; for the variables
(define (get-variables exp)
  (if (null? exp)
      '()
      (cons (caar exp) (get-variables (cdr exp)))))


; for the bindings
(define (get-bindings exp)
  (if (null? exp)
      '()
      (cons (cadr (car exp)) (get-bindings (cdr exp)))))

; this will be procedure used to evaluate let
(define (let->appl exp)
  (cons (make-lambda (get-variables (cadr exp)) (cddr exp))
        (get-bindings (cadr exp))))



#|
; prof reccomends putting these in eceval-support.scm
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;; above from book -- here are some more
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '= =)
	(list '/ /)
	(list '> >)
	(list '< <)
	(list 'list list)
        ))


|#



