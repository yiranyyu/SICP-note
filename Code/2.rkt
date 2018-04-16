#lang racket
(require (file "1.rkt"))

(define (make-rat n d)
    (let ((g (gcd (abs n) (abs d))))
        (cond ((= 0 d) (error "denominator can't be 0"))
              ((< 0 d) (cons (- n) (- d)))
              (else (cons n d)))))

(define (numer rat)
    (car rat))

(define (denom rat)
    (cdr rat))

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y)
                 (* (numer y) (denom x))))
              (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

(define (print-rat rat)
    (newline)
    (display (numer rat))
    (display "/")
    (display (denom rat)))

(define (make-point x y)
    (cons x y))

(define (x-point point)
    (car point))

(define (y-point point)
    (cdr point))

(define (make-segment start end)
    (cons start end))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))

(define (print-point point)
    (display "(")
    (display (x-point point))
    (display ".")
    (display (y-point point))
    (display ")"))

(define (print-segment segment)
    (print-point (start-segment segment))
    (display " -> ")
    (print-point (end-segment segment)))

(define (midpoint-segment segment)
    (let ((start (start-segment segment))
          (end (end-segment segment)))
         (make-point (average (x-point start) (x-point end))
                     (average (y-point start) (y-point end)))))

(define (point-left-to a b)
    (< (x-point a) (x-point b)))

(define (point-up-to a b)
    (< (y-point a) (y-point b)))

(define (make-rectangle left-up right-down)
    (cond ((and (point-left-to left-up right-down)
                (point-up-to left-up right-down)) 
           (cons left-up right-down))
          (else (error "error point for rectangle"))))

(define (get-left-up rect)
    (car rect))

(define (get-right-down rect)
    (cdr rect))

(define (rect-width rect)
    (let ((left-up (get-left-up rect))
          (right-down (get-right-down rect)))
        (min (- (x-point right-down)
                (x-point left-up))
             (- (y-point right-down)
                (y-point left-up)))))

(define (rect-length rect)
(let ((left-up (get-left-up rect))
        (right-down (get-right-down rect)))
    (max (- (x-point right-down)
            (x-point left-up))
         (- (y-point right-down)
            (y-point left-up)))))

(define (rect-area rect)
    (* (rect-width rect) (rect-length rect)))

(define (rect-perimeter rect)
    (* 2 (+ (rect-width rect)
            (rect-length rect))))
        
;; define another rectangle representation
(define (make-rectangle-real-time-compute left-up right-down)
    (cond ((and (point-left-to left-up right-down)
                (point-up-to left-up right-down)) 
           (let ((points (cons left-up right-down)))
               (cons points 
                    (cons (rect-width points) 
                          (rect-length points)))))
          (else (error "error point for rectangle"))))

(define (fast-rect-width rectangle-form-real-time-compute)
    (cdr (car rectangle-form-real-time-compute)))

(define (fast-rect-length rectangle-form-real-time-compute)
    (cdr (cdr rectangle-form-real-time-compute)))

(define (integer-cons a b)   ;; using 2^a * 3^b to represent a pair 
    (if (or (< a 0) (< b 0)) ;; both a and b required to be non-negative
        (error "nagetive argument for integer-cons: " a b)
        (* (pow 2 a)             
            (pow 3 b))))

(define (integer-car pair)
    (define (remove-2-iter count value)
        (if (= (remainder value 2) 0)
            (remove-2-iter (incre count) (/ value 2))
            count))
    (remove-2-iter 0 pair))

(define (integer-cdr pair)
    (define (remove-3-iter count value)
        (if (= (remainder value 3) 0)
            (remove-3-iter (incre count) (/ value 3))
            count))
    (remove-3-iter 0 pair))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b) ;; chet numbers 
    (lambda (f) 
        (lambda (x) ((a f) ((b f) x)))))

(define (Hannoi a b c number) ;; move number disks from a to b through c
    (define (move from to)
            (display "move from ") 
            (display from) 
            (display " to ") 
            (display to))
    (cond ((= number 1) (move a b))
          (else (Hannoi a c b (decre number))
                (move a b)
                (Hannoi c b a (decre number)))))

(define (make-interval a b)
    (cons a b))

(define (upper-bound interval)
    (max (car interval)
         (cdr interval)))

(define (lower-bound interval)
    (min (car interval)
         (cdr interval)))

(define (add-interval x y)
    (make-interval (+ (lower-bound x)
                      (lower-bound y))
                   (+ (upper-bound x)
                      (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(define (sub-interval x y)
    (make-interval (- (lower-bound x) 
                      (upper-bound y))
                   (- (upper-bound x)
                      (lower-bound y))))

(define (div-interval x y)
    (if (<= (* (lower-bound y) (upper-bound y)) 0)
        (error "cannot apply div on range cross zero")
        (mul-interval x 
            (make-interval (/ 1.0 (upper-bound y))
                           (/ 1.0 (lower-bound x))))))

(define (mul-interval-Ben x y)
    ;; for end-points of interval
    ;; return 1 if both position
    ;; -1 if both negative, otherwise 0
    (define (end-point-sign interval)
        (let ((lo (lower-bound interval))
              (up (upper-bound interval)))
            ((cond ((and (> lo 0) (> up 0)) 1)
                   ((and (< lo 0) (< up 0)) -1)
                   (else 0)))))
    (let ((e-x (end-point-sign x))
          (e-y (end-point-sign y))
          (l-x (lower-bound x))
          (l-y (lower-bound y))
          (u-x (upper-bound x))
          (u-y (upper-bound y)))
        (cond ((> e-x 0) 
               (if (>= e-y 0) 
                   (make-interval (* l-x l-y) (* u-x u-y))
                   (make-interval (* l-y u-x) (* u-y l-x))))
              ((< e-x 0) 
               (if (< e-y 0)
                   (make-interval (* l-x l-y) (* u-x u-y))
                   (make-interval (* u-x l-y) (* l-x u-y))))
              (else 
               (cond ((> e-y 0) (make-interval (* l-x l-y) (* u-x u-y)))
                     ((< e-y 0) (make-interval (* u-x l-y) (* l-x u-y)))
                     (else (make-interval (min (* l-x u-y) (* l-y u-x))
                                          (max (* l-x l-y) (* u-x u-y)))))))))

(define (make-center-percent center percent)
    (if (or (< percent 0) (= center 0))
        (error "negative percent in measurement or zero center")
        (cons (* (- 1 (/ percent 100)) center)
              (* (+ 1 (/ percent 100)) center))))

(define (center-percent-lo interval)
    (car interval))

(define (center-percent-up interval)
    (cdr interval))

(define (center interval)
  (average (center-percent-lo interval)
           (center-percent-up interval)))

(define (percent interval)
    (* 100 (/ (- (center-percent-up interval) 
                 (center interval)) 
              (center interval))))

(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))

(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval one 
                      (add-interval (div-interval one r1)
                                    (div-interval one r2)))))

(define (list-ref lst index)
    (cond ((< index 0) (error "negative index"))
          ((= index 0) (car lst))
          (else (list-ref (cdr lst) (decre index)))))


(define (list-length lst)
    (define (length-iter lst len)
        (if (null? lst)
            len
            (length-iter (cdr lst) (incre len))))
  (length-iter lst 0))

(define (append lst1 lst2)
    (if (null? lst1)
        lst2
        (cons (car lst1)
              (append (cdr lst1) lst2))))

(define (last-pair lst)
    (let ((rest (cdr lst)))
        (if (null? rest)
            lst
            (last-pair rest))))

(define (reverse lst)
    (if (null? lst)
        lst
        (append (reverse (cdr lst))
                (list (car lst)))))

(define logic-eq (lambda (a b) (not (xor a b ))))

(define (same-parity n . lst)
    (define (select is-even numbers)
        (define (iter rest result)
            (cond ((null? rest) result)
                  (else (let ((first (car rest)))
                             (if (logic-eq is-even (even? first))
                                 (iter (cdr rest) (append result (list first)))
                                 (iter (cdr rest) result))))))
        (iter numbers '()))
    (select (even? n) lst))

(define (scale-list items factor)
    (map (lambda (x) (* x factor))
         items))

(define (square-list items)
    (map (lambda (x) (square x)) 
         items))
        
(define (square-list2 items)
    (if (null? items)
        null
        (cons (square (car items)) 
              (square-list2 (cdr items)))))
        
(define (for-each proc list)
    (cond ((null? list) null)
        (else (proc (car list))
              (for-each proc (cdr list)))))

(define (count-leaves lst)
    (cond ((null? lst) 0)
          ((pair? lst) (+ (count-leaves (car lst)
                                      (cdr lst))))
          (else 1)))

(define (scale-tree tree factor)
    (cond ((null? tree) null)
          ((pair? tree) (cons (scale-tree (car tree) factor)
                              (scale-tree (cdr tree) factor)))
          (else (* factor tree))))

(define (scale-tree2 tree factor)
    (map (lambda (sub-tree)
                 (if (pair? sub-tree)
                     (scale-tree2 sub-tree factor)
                     (* sub-tree factor)))
         tree)) 

(define (square-tree tree)
    (map (lambda (sub-tree) 
                 (if (pair? sub-tree)
                     (square-tree sub-tree)
                     (square sub-tree)))
         tree))

(define (tree-map proc tree)
    (map (lambda (sub-tree)
                 (if (pair? sub-tree)
                     (tree-map proc sub-tree)
                     (proc sub-tree)))
    tree))

(define (square-tree-using-tree-map tree)
    (tree-map square tree))

(define (filter predicate sequence) ;; select elements that returns true of predicate
    (cond ((null? sequence) null)
          ((predicate (car sequence)) (cons (car sequence) 
                                            (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (range low high)
    (if (> low high)
        null
        (cons low (range (incre low) high))))

(define (enumerate-tree tree)
    (cond ((null? tree) null)
          ((pair? tree) (append (enumerate-tree (car tree))
                                (enumerate-tree (cdr tree))))
          (else (list tree))))

(define (sum-odd-squares tree)
    (accumulate + 
                0
                (map square
                     (filter odd?
                             (enumerate-tree tree)))))

(define (even-fibs n)
    (accumulate cons
                null
                (filter even?
                        (map fib
                             (range 0 n)))))

(define (flatmap make-seq seq)
    (accumulate append null (map make-seq seq)))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (map make-pair-sum
         (filter prime?
                 (flatmap
                  (lambda (i)
                          (map (lambda (j) (list i j))
                               (range 1 (decre i))))
                  (range 1 n)))))

(define (permutation s)
    (if (null? s)
        (list null)
        (flatmap (lambda (x)
                     (map (lambda (p) (cons x p))
                          (permutation (remove x s))))
                  s)))

(define (remove item sequence)
    (filter (lambda (x) (not (= x item)))
            sequence))

(define (memq item lst)
    (cond ((null? lst) false)
          ((eq? (car lst) item) lst)
          (else (memq item (cdr lst)))))

(define (my_equal? lst_a lst_b)
    (cond ((null? lst_a) (null? lst_b))
          ((null? lst_b) false)
          (else (let ((first_a (car lst_a))
                      (first_b (car lst_b)))
                    (cond ((pair? first_a) (and (pair? first_b) 
                                                (my_equal? first_a first_b)
                                                (my_equal? (cdr lst_a) 
                                                           (cdr lst_b))))
                          (else (and (not (pair? first_b))
                                     (eq? first_a first_b)
                                     (my_equal? (cdr lst_a)
                                                (cdr lst_b)))))))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum 
                         (make-product (multiplier exp)
                                       (deriv (multiplicand exp) var))
                         (make-product (deriv (multiplier exp) var)
                                       (multiplicand exp))))
        ((exponentiation? exp) (make-product
                                (make-product (exponent exp)
                                            (deriv (base exp) var))
                                (make-exponentiation 
                                    (base exp)
                                    (decre (exponent exp)))))
        (else (error "unknown expression type -- DERIV" exp))))

(define (variable? x)
    (symbol? x))

(define (same-variable? x y)
    (and (variable? x) 
         (variable? y)
         (eq? x y)))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

(define (make-sum-list lst)
    (cons '+ lst))      

(define (make-product m1 m2)
    (cond ((or (=number? m1 0)
               (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1)
                (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

;; input: (list 'a 'b 'c 'd)
;; output:(list '* 'a 'b 'c 'd)
(define (make-product-list lst)
    (cons '* lst))

(define (make-exponentiation base expo)
    (cond ((=number? base 0) 0)
          ((=number? expo 0) 1)
          ((and (number? base)
                (number? expo) (pow base expo)))
          (else (list '** base expo))))

(define (sum? x)
    (and (pair? x)
         (eq? (car x) '+ )))

(define (addend x)  ; second item in a sum expression
    (cadr x))

(define (augend x)
    (let ((rest (cddr x)))
        (cond ((= 1 (length rest)) (car rest))
              (else  (make-sum-list rest)))))

(define (product? x)
    (and (pair? x)
         (eq? (car x) '* )))

(define (multiplier p)  ; second item in product expression
    (cadr p))

(define (multiplicand p)
    (let ((rest (cddr p)))
         (cond ((= 1 (length rest)) (car rest))
               (else (make-product-list rest)))))

(define (exponentiation? x)
    (and (pair? x)
         (eq? (car x) '** )))
    
(define (base e)
    (cadr e))

(define (exponent e)
    (caddr e))

(define (=number? arg num)
    (and (number? arg)
         (= arg num)))

(define (make-set lst)
    (if (pair? lst)
        lst
        (list lst)))

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

(define (intersection-set set1 set2)
    (cond ((or (null? set1)
               (null? set2)) '())
          ((element-of-set? (car set1) set2) 
           (cons (car set1)
               (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else (append (filter
                            (lambda (item) (not (element-of-set? item set2))) 
                            set1) 
                        set2))))

(define (make-ordered-number-set lst)
    (cond ((pair? lst) (union-ordered-number-set (list (car lst)) 
                                                 (cdr lst)))
          (else (list lst))))

(define (element-of-ordered-number-set? x set)
    (cond ((null? set) false)
          ((= x (car set)) true)
          ((< x (car set)) false)
          (else (element-of-ordered-number-set? x (cdr set)))))

(define (intersection-ordered-number-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        ((let ((x1 (car set1))
               (x2 (car set2)))
            (cond ((= x1 x2) 
                   (cons x1
                        (intersection-ordered-number-set 
                            (cdr set1)
                            (cdr set2))))
                  ((< x1 x2)
                   (intersection-ordered-number-set (cdr set1) set2))
                  (else (intersection-ordered-number-set set1 (cdr set2))))))))

(define (union-ordered-number-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set2)
          (else
           (let ((x1 (car set1))
                 (x2 (car set2)))
              (cond ((= x1 x2) 
                     (cons x1
                        (union-ordered-number-set (cdr set1)
                                                  (cdr set2))))
                    ((< x1 x2) 
                     (cons x1
                        (union-ordered-number-set (cdr set1)
                                                  set2)))
                    (else 
                     (cons x2 
                        (union-ordered-number-set set1
                                                  (cdr set2)))))))))

(define (adjoin-ordered-number-set x set)
    (cond ((null? set) (make-ordered-number-set x))
          (else (let ((x1 (car set)))
                     (cond ((= x x1) set)
                           ((< x x1) (cons x set))
                           (else (cons x1 (adjoin-ordered-number-set x (cdr set)))))))))

(define (root tree)
    (car tree))

(define (left-branch tree)
    (cadr tree))

(define (right-branch tree)
    (caddr tree))

(define (make-leaf value)
    (list value '() '()))

(define (make-tree root left right)
    (list root left right))

(define (element-of-tree? x tree)
    (cond ((null? tree) false)
          (else 
            (let ((root (root tree)))
                 (cond ((= x root) true)
                       ((< x root) (element-of-tree? x 
                                                    (left-branch tree)))
                       (else (element-of-tree? x 
                                              (right-branch tree))))))))

(define (adjoin-tree x tree)
    (cond ((null? tree) (make-tree x '() '()))
          (else
            (let ((root (root tree)))
                 (cond ((= x root) 
                        tree)
                       ((< x root) 
                        (make-tree root 
                                   (adjoin-tree x (left-branch tree))
                                   (right-branch tree)))
                       (else
                        (make-tree root
                                   (left-branch tree)
                                   (adjoin-tree x (right-branch tree)))))))))


(define (tree->list-1 tree)
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons (root tree)
                      (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons (root tree)
                                (copy-to-list (right-branch tree)
                                              result-list)))))
  (copy-to-list tree '() ))

(define (list->tree lst)
    (car (partial-tree lst (length lst))))

;; return a pair
;; first part is a tree of the first n elements
;; second part is the rest elements
(define (partial-tree lst n)
    (if (= n 0)
        (cons '() lst)
        (let ((left-size (quotient (decre n) 2)))
            (let ((left-result (partial-tree lst left-size)))
                (let ((left-tree (car left-result))
                      (non-left-lst (cdr left-result))
                      (right-size (- n (incre left-size))))
                    (let ((this-root (car non-left-lst))
                          (right-result (partial-tree (cdr non-left-lst)
                                                      right-size)))
                        (let ((right-tree (car right-result))
                               (remaining-lst (cdr right-result)))
                            (cons (make-tree this-root left-tree right-tree)
                                  remaining-lst))))))))

(define set1 (make-set (list 1 2 3)))
(define set2 (make-set (list 3 4 5)))
(define test adjoin-ordered-number-set)
(define tree (make-tree 4 (make-tree 2 (make-tree 1 '() '())
                                       (make-tree 3 '() '()))
                          (make-tree 6 (make-tree 5 '() '())
                                       (make-tree 7 '() '()))))