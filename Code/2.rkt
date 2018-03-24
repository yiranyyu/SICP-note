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

(define r1 (make-interval 1 2))
(define r2 (make-interval 2 4))
(div-interval r1 r1)
(div-interval r1 r2)
(par1 r1 r2)
(par2 r1 r2)