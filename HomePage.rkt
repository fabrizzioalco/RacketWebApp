#lang web-server/insta
(static-files-path "htdocs")
(struct titulos (posts) #:mutable)
(struct post( id title result))

(require "algoritmos_tlp.rkt")

(define Home (titulos (list
                       (post 1 "Merge Sort" "")
                       (post 2 "Elemento máximo de una lista" "")
                       (post 3 "Elemento mínimo de una lista" "")
                       (post 4 "Generador de números primos" "")
                       (post 5 "Suma de los primeros N números impares" "")
                       (post 6 "Calcular desviación estándar" ""))))

;;Empezamos servidor renderizando el Home
(define (start request)
  (define home
    (cond [(can-parse-post? (request-bindings request))
           (cons (parse-post (request-bindings request)) Home)] [else Home]))
  (render-Home request))

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
             (p (a ([href, (embed/url max-handler)]) "Elemento máximo de una lista"))
             (p (a ([href, (embed/url min-handler)]) "Elemento mínimo de una lista"))
             (p (a ([href, (embed/url prime-handler)]) "Generador de números primos"))
             (p (a ([href, (embed/url sum-handler)]) "Suma de los primeros N números impares"))
             (p (a ([href, (embed/url sigma-handler)]) "Calcular la desviación estándar"))
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
                    (p "La lista ordenada es: ", (list->string (listanum->lista (merge-sort (lista->listanum (string->list input))))))
                    (p (a ([href, (embed/url back-handler)]) "Back to main page"))
                    )))))
         (else
          (response/xexpr
           `(html (head (title "Merge Sort"))
                  (body
                   (h1 "Merge Sort")
                   (h3 "Ingrese una lista de numeros: ")
                   (form
                    (input ((name "~")))
                    (input((type "Submit"))))
                   (p (a ([href, (embed/url back-handler)]) "Back to main page "))
                   ))))))
  (define (back-handler request)
    (render-Home request))
  (send/suspend/dispatch response-generator ))


(define (max-handler request)
  (define (response-generator embed/url)
    (define bindings (request-bindings request))
    (cond((exists-binding? '~ bindings)
          (define input (extract-binding/single '~ bindings))
          (response/xexpr
           `(html (head (title "Elemento máximo"))
                  (body
                   (h1 "Elemento máximo de una lista")
                   (body
                    (p "El elemento máximo de la lista es: ",  (number->string (max-in-list (lista->listanum (string->list input)))))
                    (p (a ([href, (embed/url back-handler)]) "Back to main page")))))))
         (else
          (response/xexpr
           `(html (head (title "Elemento máximo"))
                  (body
                   (h1 "Elemento máximo de una lista")
                   (h3 "Ingrese una lista de numeros: ")
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
           `(html (head (title "Elemento mínimo"))
                  (body
                   (h1 "Elemento mínimo de una lista")
                   (body
                    (p "El elemento mínimo de la lista es: ",  (number->string(min-in-list (lista->listanum (string->list input)))))
                    (p (a ([href, (embed/url back-handler)]) "Back to main page"))) ) )))
         (else
          (response/xexpr
           `(html (head (title "Elemento mínimo"))
                  (body
                   (h1 "Elemento mínimo de una lista")
                   (h3 "Ingrese una lista de numeros:")
                   (form
                    (input ((name "~")))
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
          (define number1 (extract-binding/single '~ bindings))
          (define number2 (extract-binding/single '& bindings))
          (response/xexpr
           `(html (head (title "Generador de números primos"))
                  (body
                   (h1 "Generador de números primos")
                   (body
                    (p "Los números primos son: ", (~a (primes (string->number number1) (string->number number2))))
                    (p (a ([href, (embed/url back-handler)]) "Back to main page"))))))) 
         (else
          (response/xexpr
           `(html (head (title "Generador de números primos"))
                  (body
                   (h1 "Generador de números primos")
                   (h3 "Ingresa dos numeros:")
                   (form
                    (input ((name "~")))
                    (input ((name "&")))
                    (input((type "Submit"))))
                   (p (a ([href, (embed/url back-handler)]) "Back to main page"))
                   ))))))
  (define (back-handler request)
    (render-Home request))
  (send/suspend/dispatch response-generator))

(define (sum-handler request)
  (define (response-generator embed/url)
    (define bindings (request-bindings request))
    (cond((exists-binding? '~ bindings)
          (define input (extract-binding/single '~ bindings))
          (response/xexpr
           `(html (head (title "Suma de los números impares"))
                  (body
                   (h1 "Suma de los números impares hasta N")
                   (body
                    (p "La suma de los números es: ", (suma-impares (string->number input)))
                    (p (a ([href, (embed/url back-handler)]) "Back to main page")))))))
         (else
          (response/xexpr
           `(html (head (title "Suma de los números impares"))
                  (body
                   (h1 "Suma de los números impares hasta N")
                   (h3 "Ingrese un número (N): ")
                   (form
                    (input ((name "~")))
                    (input((type "Submit"))))
                   (p (a ([href, (embed/url back-handler)]) "Back to main page"))
                   ))))))
  (define (back-handler request)
    (render-Home request))
  (send/suspend/dispatch response-generator))

(define (sigma-handler request)
  
  (define bindings (request-bindings request))
  (define (response-generator embed/url)
    (cond((exists-binding? '~ bindings)
          (define input (extract-binding/single '~ bindings))
        
          (response/xexpr
           `(html (head (title "Desvación Estándar"))
                  (body
                   (h1 "Calcular la desviación estándar")
                   (body
                    (p "La desviación estándar es: ", (number->string (sigma (lista->listanum (string->list input)))))
                    (p (a ([href, (embed/url back-handler)]) "Back to main page"))
                    )))))
         (else
          (response/xexpr
           `(html (head (title "Desviación Estándar"))
                  (body
                   (h1 "Calcular la desviación estándar")
                   (h3 "Ingrese una lista de numeros: ")
                   (form
                    (input ((name "~")))
                    (input((type "Submit"))))
                   (p (a ([href, (embed/url back-handler)]) "Back to main page "))
                   ))))))
  (define (back-handler request)
    (render-Home request))
  (send/suspend/dispatch response-generator ))


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


;; HELPERS

(define (lista->listanum l)
  (cond [(empty? l) empty]
        [else
         (cons (- (char->integer (first l)) 48) (lista->listanum (rest l)))]))

(define (listanum->lista l)
  (cond [(empty? l) empty]
        [else
         (cons (integer->char (+ 48 (first l))) (listanum->lista (rest l)))]))



