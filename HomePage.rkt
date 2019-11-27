#lang web-server/insta
(struct titulos (posts) #:mutable)
(struct post(title))

(define Home (list
              (post "Merge Sort")
              (post "Max element")
              (post "Min element")
              (post "Prime number")))

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
           ,(render-posts items)))))
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

(define (return-title-as-list items)
  `(div ((class "home-list"))
        `(ul ,@map render-as-item items)))
(define (render-as-item items)
  `(li, items))


