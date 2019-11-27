#lang web-server/insta
(struct titulos (posts) #:mutable)
(struct post( id title))



(define Home (titulos (list
              (post 1 "Merge Sort")
              (post 2 "Max element")
              (post 3 "Min element")
              (post 4 "Prime number"))))

;;Empezamos servidor renderizando el Home
(define (start request)
  (define home
    (cond [(can-parse-post? (request-bindings request))
           (cons (parse-post (request-bindings request)) Home)] [else Home]))

(render-Home home request))
;;checamos que tengamos datos en los posts.
(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)))


;;extraemos los datos de la estructura del post
  (define (parse-post bindings)
    (Home (extract-binding/single 'title bindings)))
    
;;estructura del html de la pagina principal
(define (render-Home items req)
  ;;rresponse-generator embed/url crea nuevos urls para las rutas que se le asigne 
 (define (response-generator embed/url)
    (response/xexpr 
   `(html (head (title "Algorithms"))
          (body
           (h1 "Algoritmos")
           (p (a ([href, (embed/url merge-handler)]) "Merge Sort"))
           (p (a ([href, (embed/url merge-handler)]) "Max Element"))
           (p (a ([href, (embed/url merge-handler)]) "Min Element"))
           (p (a ([href, (embed/url merge-handler)]) "Prime number"))
           ))))

  (send/suspend/dispatch response-generator))


(define (merge-handler request)
  
    (response/xexpr
     `(html (head (title "Merge Sort"))
            (body
             (h1 "Merge Sort")
             (h3 "Ingrese una lista de numeros")
             (input (name "~"))
             (button(name "Send"))
             ))))

(define (max-handler request)
  
    (response/xexpr
     `(html (head (title "Max Element"))
            (body
             (h1 "Max Element")
             (h3 "Ingrese una lista de numeros")
             (input (name "~"))
             (button(name "Send"))
             ))))

(define (min-handler request)
  
    (response/xexpr
     `(html (head (title "Min element"))
            (body
             (h1 "Prime number")
             (h3 "Ingrese una lista de numeros")
             (input (name "~"))
             (button(name "Send"))
             ))))
(define (prime-handler request)
  
    (response/xexpr
     `(html (head (title "Prime number"))
            (body
             (h1 "Prime number")
             (h "Ingrese dos numeros")
             (input (name "1"))
             (input (name "2"))
             (button(name "Send"))
             ))))



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


