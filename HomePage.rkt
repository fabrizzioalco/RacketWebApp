#lang web-server/insta
(static-files-path "htdocs")
(struct titulos (posts) #:mutable)
(struct post( id title result))



(define Home (titulos (list
              (post 1 "Merge Sort" "")
              (post 2 "Max element" "")
              (post 3 "Min element" "")
              (post 4 "Prime number" ""))))

;;Empezamos servidor renderizando el Home
(define (start request)
  (define home
    (cond [(can-parse-post? (request-bindings request))
           (cons (parse-post (request-bindings request)) Home)] [else Home]))

(render-Home  request))
;;checamos que tengamos datos en los posts.
(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)))


;;extraemos los datos de la estructura del post
  (define (parse-post bindings)
    (Home (extract-binding/single 'title bindings)))
    
;;estructura del html de la pagina principal
(define (render-Home  req)
  ;;rresponse-generator embed/url crea nuevos urls para las rutas que se le asigne 
 (define (response-generator embed/url)
    (response/xexpr 
   `(html (head (title "Algorithms"))
          (body
           (h1 "Algoritmos")
           (link((rel "stylesheet")
                  (href "/test-css.css")
                  (type "text/css")))
           (p (a ([href, (embed/url merge-handler)]) "Merge Sort"))
           (p (a ([href, (embed/url max-handler)]) "Max Element"))
           (p (a ([href, (embed/url min-handler)]) "Min Element"))
           (p (a ([href, (embed/url prime-handler)]) "Prime number"))
           ))))

  (send/suspend/dispatch response-generator))


(define (merge-handler request)
  
  (define bindings (request-bindings request))
  (define (response-generator embed/url)
  (cond((exists-binding? '~ bindings)
        (define input (extract-binding/single '~ bindings))
        
       (response/xexpr
        `(html (head (title "Merge Sort"))
               (body
                (h1 "Merge Sort")
                (body
                 (p "La lista es: ", (list->string (listanum->lista (merge-sort (lista->listanum (string->list input))))))
                 (p (a ([href, (embed/url back-handler)]) "Back to main page"))
                 )))))
       (else
       (response/xexpr
     `(html (head (title "Merge Sort"))
            (body
             (h1 "Merge Sort")
             (h3 "Ingrese una lista de numeros")
             (form
              (input ((name "~")))
             (input((type "Submit"))))
             (p (a ([href, (embed/url back-handler)]) "Back to main page "))
             ))))))
  (define (back-handler request)
    (render-Home request))
  (send/suspend/dispatch response-generator ))
  
 

(define (lista->listanum l)
  (cond [(empty? l) empty]
        [else
         (cons (- (char->integer (first l)) 48) (lista->listanum (rest l)))]))

(define (listanum->lista l)
  (cond [(empty? l) empty]
        [else
         (cons (integer->char (+ 48 (first l))) (listanum->lista (rest l)))]))

(define (max-handler request)
  (define (response-generator embed/url)
  (define bindings (request-bindings request))
  (cond((exists-binding? '~ bindings)
        (define input (extract-binding/single '~ bindings))
       (response/xexpr
        `(html (head (title "Max Element"))
               (body
                (h1 "Max Element")
                (body
                 (p "La lista es: ",  (number->string (max-in-list (lista->listanum (string->list input)))))
                 (p (a ([href, (embed/url back-handler)]) "Back to main page")))))))
       (else
       (response/xexpr
     `(html (head (title "Max Element"))
            (body
             (h1 "Max Element")
             (h3 "Ingrese una lista de numeros")
             (form
              (input ((name "~")))
             (input((type "Submit"))))
             (p (a ([href, (embed/url back-handler)]) "Back to main page"))
             ))))))


  (define (back-handler request)
    (render-Home request))
  (send/suspend/dispatch response-generator))

(define (min-handler request)
  (define (response-generator embed/url)
  (define bindings (request-bindings request))
  (cond((exists-binding? '~ bindings)
        (define input (extract-binding/single '~ bindings))
       (response/xexpr
        `(html (head (title "Min Number"))
               (body
                (h1 "Merge Sort")
                (body
                 (p "El numero es: ",  (number->string(min-in-list (lista->listanum (string->list input)))))
                 (p (a ([href, (embed/url back-handler)]) "Back to main page"))) ) )))
       (else
       (response/xexpr
     `(html (head (title "Min N"))
            (body
             (h1 "Merge Sort")
             (h3 "Ingrese una lista de numeros")
             (form
              (input ((name "1")))
             (input((type "Submit"))))
             (p (a ([href, (embed/url back-handler)]) "Back to main page"))
             ))))))
(define (back-handler request)
    (render-Home request))
  (send/suspend/dispatch response-generator))



(define (prime-handler request)
  (define (response-generator embed/url)
  (define bindings (request-bindings request))
  (cond((exists-binding? '~ bindings)
        (define input (extract-binding/single '(number1) bindings))
        (define number2 (extract-binding/single '(number2) bindings))
       (response/xexpr
        `(html (head (title "Prime number"))
               (body
                (h1 "Merge Sort")
                (body
                 (p "El numero es: ",  (listanum->lista(primes ((string->number(input) string->number(number2))))))
                 (p (a ([href, (embed/url back-handler)]) "Back to main page"))) ) )))
       (else
       (response/xexpr
     `(html (head (title "Prime number"))
            (body
             (h1 "Prime number")
             (h3 "Ingresa dos numeros")
             (form
              (input ((name "number1")))
              (input ((name "number2")))
             (input((type "Submit"))))
             (p (a ([href, (embed/url back-handler)]) "Back to main page"))
             ))))))
  (define (back-handler request)
    (render-Home request))
  (send/suspend/dispatch response-generator))


;;renderizamos los items que tiene el post (single items from a structure )
(define (render-post items)
  `(div ((class "home"))
        (p, (post-title items))))
  
  

;;renderizamos listas de objetos.
(define (render-posts items)
  `(div ((class "home-items"))
        ,@(map render-post items)))

(define (render-postit a-post embed/url)
  (define (view-post-handler request)
    (render-Home a-post request))
  `(div ((class "post"))
        (a ((href ,(embed/url view-post-handler)))
           ,(post-title a-post))
        ))
;otra vez para renderizar
(define (render-postss embed/url)
  (define (render-post/embed/url a-post)
    (render-postit a-post embed/url))
  `(div ((class "postsss"))
        ,@(map render-post/embed/url (titulos-posts Home))))

(define (return-title-as-list items)
  `(div ((class "home-list"))
        `(ul ,@map render-as-item items)))
(define (render-as-item items)
  `(li, items))


;-------------Algoritmos--------------------
  (require racket/list)

;; === ALGORITMOS ===

;; 1 --- MERGE SORT

;; (listof Number) -> (listof Number)
;; sort a list using the merge sort algorithm.


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


;;(define (primes n1 n2) empty) ;stub

(define (primes n1 n2)
  (local [(define LIST (filter (lambda (x) (> x n1)) (build-list n2 add1)))]
    (filter prime? LIST)))

;; Number -> Boolean
;; return true if given number is a prime number


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


;; (define (max-in-list lon) 0) ;stub

(define (max-in-list lon)
  (cond [(empty? lon) false]
        [else
         (last (merge-sort lon))]))

;; ----------------------------------------------------------------------------

;; 4 ----- MIN ELEMENT

;; (listof Number) -> Number
;; returns min element in a given list, false if list is empty


;; (define (max-in-list lon) 0) ;stub

(define (min-in-list lon)
  (cond [(empty? lon) false]
        [else
         (first (merge-sort lon))]))

