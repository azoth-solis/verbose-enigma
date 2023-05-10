#lang slideshow

;;; Range:
(define (range x)
  (define (countdown x)
    (if (= x 1)
        '(1)
        (cons x (countdown (- x 1)))))
  (reverse (countdown x)))

;;; Filter:
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;;; Sieve:
(define (eratosthenes x)
  (define sequence (cdr (range x)))
  (define iteration-count (inexact->exact (floor (sqrt (car (reverse sequence))))))
  (define (remove-multiples integer sequence)
    (cons integer (filter (lambda (x) (not (= 0 (remainder x integer)))) sequence)))
  (define (sieve iteration-count divisors sequence)
    (if (= iteration-count 1)
      sequence
      (sieve (- iteration-count 1) (cdr divisors) (remove-multiples iteration-count sequence))))
  (cons 1 (sieve iteration-count (reverse (range iteration-count)) sequence)))

;;; Jumps:
(define (jumps sequence)
  (define (jumps-iter sequence result)
    (if (= (length sequence) 2)
        (cons (- (- (car sequence) (car (cdr sequence)))) result)
        (jumps-iter (cdr sequence) (cons (- (- (car sequence) (car (cdr sequence)))) result))))
  (reverse (jumps-iter sequence '())))

;;; Tower:
(define (tower sequence)
  (define (level number)
    (if (= number 1)
        (colorize (filled-rectangle 4 4) "red")
        (hc-append (colorize (filled-rectangle 4 4) "red") (level (- number 1)))))
  (define (tower-iter count length sequence)
    (if (= count length)
        (level (car sequence))
        (vc-append (level (car sequence)) (tower-iter (+ count 1) length (cdr sequence)))))
  (tower-iter 1 (length sequence) sequence))
