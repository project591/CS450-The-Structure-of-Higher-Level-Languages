(begin
  
(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

(define data (with-input-from-file "dist.dat" read-file))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

; this will insert the data from file
; into a table
(define (inserter data table)
  (if (null? data)
      'ok
      (begin
        (insert! (car (car data))
                  (cadr (car data)) (caddr (car data)) table)
         (inserter (cdr data) table))))

; this is a reverse table I use to get
; the cost of the nodes. 
(define (inserter-rv data table)
   (if (null? data)
      'ok
      (begin
        (insert! (car (car data)) (caddr (car data))
                 (cadr (car data))  table)
         (inserter-rv (cdr data) table))))

; 2d table
(define node-table
  (list '*table*))

(inserter data node-table) ; insert data into 2-d table


(define rv
  (list '*table*))

(inserter-rv data rv) ; this table will retrieve cost of node. 

; helper procedure for retriving
; the children of nodes
(define (children node data)
  (cond ((null? data) '())                                            
        (else (cons (assoc node data) (children node (cdr data))))))

; helps in removing duplicates
; when it try to create a children list for nodes. 
(define (remove-duplicates l)
  (cond ((null? l)
         '())
        ((memq (car l) (cdr l)) 
         (remove-duplicates (cdr l)))
        (else
         (cons (car l) (remove-duplicates (cdr l))))))

; this helps in getting the
; children node of each node
(define (c-node element)
      (map cadr element))

; remove element from list
(define (remove list1 element)
  (filter (lambda (x) (not (equal? x element))) list1))

; from the textbook. the filter procedure. 
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


; this procedure helps me get
; the node with the smallest cost. 
(define m-min
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) (car l))
      ;((eq? l #f) '())
      (#t (let ((a (car l))
                (b (m-min (cdr l))))
            (if (< b a) b a))))))

; inserts 100000 into node table
; for nodes with no children. 
(define (override-val node child)

  ; checking to see if the children does have children
  ; if it does not, then the value in table will be changed
  ; to 1 million for that particular node.
  (let ((children (c-node (remove
                           (remove-duplicates (children child data)) #f))))

    (if (and (not (eq? child 'end)) (eq? children '()))
        (insert! node child 100000000 node-table))
    
    ;   (display "inside the override: ")
    ;   (display child)
    ;   (display " children; ")
    ;   (display children)
    ;   (newline)
    ))

; helper function to test if children is not null?
(define (child-check node children)
  (if (null? children)
      '()
      (override-val node (car children)))
  )


(define (cost node children)

  (child-check node children) ; testing to see that children is not null

  ;(display children)
   (cond ((eq? children '()) '()) ; maybe ok?
        ((eq? children 'end) '())
        (else (cons (lookup node
                            (car children) node-table)
                    (cost node (cdr children)))))        
  )

(define (naive-path node)
   
  ;(display "Node: ")
  ;(display node)
  ;(newline)
  ;(c-node(delete-element (remove-duplicates (children 'p2 data))))
   
  (let ((children (c-node (remove
                           (remove-duplicates (children node data)) #f))))  
    ; (display "displaying the children: ")
    ; (display children)
    ;  (newline )
    ;(m-min (get-val node children))

    (let ((lister (cost node children)))

      ; (display lister)  
      (if (eq? node 'end) 
          0
          (+ (m-min lister) (naive-path (lookup node (m-min lister) rv)))
          ))))

(naive-path 'start)
)
