;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname algoritmos_tlp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/list)
(require racket/base)

;; === ALGORITMOS ===

;; 1 --- MERGE SORT

;; (listof Number) -> (listof Number)
;; sort a list using the merge sort algorithm.
(check-expect (merge-sort (list 1 2 3 4)) (list 1 2 3 4))
(check-expect (merge-sort (list 4 2 1 3)) (list 1 2 3 4))

;;(define (merge-sort lon) empty) ;stub

(define (merge-sort lon)
  (local [(define LEN (length lon))
          (define HALF (floor (/ LEN 2)))]
    
    (if (> LEN 1)
        (merge (merge-sort (take lon HALF)) (merge-sort (drop lon HALF)))
        lon)))

;; (listof Number) (listof Number) -> (listof Number)
;; takes two lists and merges them in sorted order.

;;(define (merge lon1 lon2) empty) ;stub

(define (merge lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(empty? lon2) lon1]
        [(< (first lon1) (first lon2))
         (cons (first lon1) (merge (rest lon1) lon2))]
        [else
         (cons (first lon2) (merge lon1 (rest lon2)))]))

;; ----------------------------------------------------------------------------

;; 2 ----- PRIME NUMBERS

;; Number1 Number2 -> (listof Number)
;; return all prime numbers between the two numbers given, empty otherwise.
;; ASSUME: Number1 < Number2
(check-expect (primes 3 10) (list 5 7))
(check-expect (primes 1 30) (list 2 3 5 7 11 13 17 19 23 29))
(check-expect (primes 8 9) empty)

;;(define (primes n1 n2) empty) ;stub

(define (primes n1 n2)
  (local [(define LIST (filter (lambda (x) (> x n1)) (build-list n2 add1)))]
    (filter prime? LIST)))

;; Number -> Boolean
;; return true if given number is a prime number
(check-expect (prime? 2) true)
(check-expect (prime? 5) true)
(check-expect (prime? 10) false)

;; (define (prime? n) false) ;stub

(define (prime? n)
  (local [(define LON (build-list (inexact->exact (floor (sqrt n))) add1))
          (define LOM (filter (lambda (x) (equal? (/ n x) (floor (/ n x)))) (rest LON)))]
    (if (empty? LOM)
        true
        false)))

;; ----------------------------------------------------------------------------

;; 3 ----- MAX ELEMENT

;; (listof Number) -> Number or False
;; returns max element in a given list, false if list is empty
(check-expect (max-in-list empty) false)
(check-expect (max-in-list (list 1 2 3 4 5)) 5)
(check-expect (max-in-list (list 10 2323 23 112 33 456)) 2323)

;; (define (max-in-list lon) 0) ;stub

(define (max-in-list lon)
  (cond [(empty? lon) false]
        [else
         (last (merge-sort lon))]))

;; ----------------------------------------------------------------------------

;; 4 ----- MIN ELEMENT

;; (listof Number) -> Number
;; returns min element in a given list, false if list is empty
(check-expect (min-in-list empty) false)
(check-expect (min-in-list (list 1 2 3 4 5)) 1)
(check-expect (min-in-list (list 10 2323 23 112 33 456)) 10)

;; (define (max-in-list lon) 0) ;stub

(define (min-in-list lon)
  (cond [(empty? lon) false]
        [else
         (first (merge-sort lon))]))

;; ----------------------------------------------------------------------------

;; 5 ------ SUMA IMPARES

;; Number -> Number
;; suma de los primeros n numeros positivos impares
(check-expect (suma-impares 1) 1)
(check-expect (suma-impares 3) (+ 1 3))
(check-expect (suma-impares 10) (+ 1 3 5 7 9))
(check-expect (suma-impares 20) (+ 1 3 5 7 9 11 13 15 17 19))

;; (define (suma-impares n) 0) ;;stub

(define (suma-impares n)
  (local [(define NUM (build-list n add1))
          (define IMPARES (filter odd? NUM))]
    (foldr + 0 IMPARES)))

;; ----------------------------------------------------------------------------

;; 6 ------ DESVIACIÓN ESTÁNDAR

;; (listof Number) -> Number
;; calcular la desviación estándar de una lista de números.
(check-expect (sigma (list 10 15 10 15 10 15)) 2.5)

(define (sigma lon)
  (local [(define LEN (length lon))
          (define MEDIA (/ (foldr + 0 lon) LEN))]
    (sqrt (* (/ 1 LEN) (foldr + 0 (map (lambda (x) (* (- x MEDIA) (- x MEDIA))) lon))))))

;; export functions
(provide (all-defined-out))