#lang web-server/insta

;;
(struct post((list)))

(define (start request)
  (render-merge-page))
(define (can-parse-post? bindings)
  (and (exists-binding? '(list) bindings)))


;;extraemos los datos de la estructura del post
  (define (parse-post bindings)
    (merge (extract-binding/single '(list) bindings)))

(define (render-merge-page lista)
  
    (response/xexpr
     `(html (head (title "Merge Sort"))
            (body
             (h1 "Ingresa tus datos")))))

;;;algoritmo
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