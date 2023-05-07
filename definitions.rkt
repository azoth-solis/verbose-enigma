#lang slideshow

;;; Range:
(define (range x)
  (define (countdown x)
    (if (= x 1)
        '(1)
        (cons x (countdown (- x 1)))))
  (reverse (countdown x)))

;;; Sum:
(define (sum sequence)
  (if (null? sequence)
      0
      (+ (car sequence) (sum (cdr sequence)))))

;;; Filter HOF:
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;;; Sieve of Eratosthenes:
(define (eratosthenes x)
  (define sequence (cdr (range x)))
  (define limit (inexact->exact (floor (sqrt (car (reverse (cdr (range x))))))))
  (define (remove-multiples integer sequence)
    (cons integer (filter (lambda (x) (not (= 0 (remainder x integer)))) sequence)))
  (define (sieve limit table sequence)
    (if (= limit 1)
      sequence
      (sieve (- limit 1) (cdr table) (remove-multiples limit sequence))))
  (cons 1 (sieve limit (reverse (range limit)) sequence)))

;;; Triangle:
(define (triangle x)
  (/ (* x (+ 1 x)) 2))
;;; Square:
(define (square x)
  (expt x 2))

;;; Tetrahedron:
(define (tetrahedron x)
  (sum (map triangle (range x))))
;;; Pyramid:
(define (pyramid x)
  (sum (map square (range x))))
;;; Octahedron:
(define (octahedron x)
  (/ (* x (+ 1 (* 2 (square x)))) 3))

;;; Tower:
(define (tower sequence)
  (define (level number)
    (if (= number 1)
        (colorize (filled-rectangle 2 2) "red")
        (hc-append (colorize (filled-rectangle 2 2) "red") (level (- number 1)))))
  (define (tower-iter count length sequence)
    (if (= count length)
        (level (car sequence))
        (vc-append (level (car sequence)) (tower-iter (+ count 1) length (cdr sequence)))))
  (tower-iter 1 (length sequence) sequence))