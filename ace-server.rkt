#lang racket

#| ace-server.rkt https://github.com/rurbina/ace-server |#

(provide ace-server)

(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         unstable/struct
         xml
         racket/match
         racket/list
         racket/date
         racket/path
         scribble/decode)

(define-namespace-anchor anchor)

(define (parse-post-data data)
  (cond
   ([boolean? data] '())
   ([bytes? data]
    (for/list ([pair (string-split (bytes->string/utf-8 data) "&")])
      (let ([splitted (string-split pair "=")]
            [key null]
            [value null])
        (set! key (car splitted))
        (set! value (cdr splitted))
        `(,(string->symbol key) . ,value)
        )))))

(define ace-get
  (syntax-rules ()
    [(ace-get sym)
     (cond ([string? sym]
            (hash-ref ace-query-hash (string->symbol sym)))
           ([symbol? sym]
            (hash-ref ace-query-hash 'sym 
                      (hash-ref ace-query-hash sym
                                (string-append "[" (symbol->string sym) " not defined]" ))))
           )]
    [(ace-get) ace-query-hash]))

(define ace-post
  (syntax-rules ()
    [(ace-post sym) 
     (cond ([string? sym]
            (hash-ref ace-query-post-hash (string->symbol sym) null))
           ([symbol? sym]
            (hash-ref ace-query-post-hash 'sym
                      (hash-ref ace-query-post-hash sym
                                (string-append "[" (symbol->string sym) " not defined]" ))))
           )]
    [(ace-post) ace-query-post-hash]))

;; servlet processing thread
(define (ace-servlet req)
  (let ([template-path (string->path ".")]
        [tns (make-empty-namespace)]
        [output null]
        [parsed-post (parse-post-data (request-post-data/raw req))]
        [response-code 200]
        [response-const #"OK"])
    ;; resolve the path
    (for ([p (url-path (request-uri req))])
      (set! template-path (build-path template-path (path/param-path p))))
    (set! template-path
          (find-relative-path (current-directory) 
                              (simple-form-path template-path)))
    ;; init the template namespace and load the script
    (parameterize ((current-namespace tns))
      (namespace-attach-module (namespace-anchor->empty-namespace anchor) 'racket)
      (namespace-attach-module (namespace-anchor->empty-namespace anchor) 'web-server/servlet)
      (namespace-attach-module (namespace-anchor->empty-namespace anchor) 'web-server/templates)
      (namespace-require 'racket)
      (namespace-require 'web-server/servlet)
      (namespace-require 'web-server/templates)
      (namespace-require 'xml))
    (namespace-set-variable-value! 'ace-request req #f tns)
    (namespace-set-variable-value! 'ace-query (url-query (request-uri req)) #f tns)
    (namespace-set-variable-value! 'ace-query-hash null #f tns)
    (namespace-set-variable-value! 'ace-query-post parsed-post #f tns)
    (namespace-set-variable-value! 'ace-query-post-hash null #f tns)
    (eval `(define-syntax ace-get ,ace-get) tns)
    (eval `(define-syntax ace-post ,ace-post) tns)
    (eval '(set! ace-query-hash (make-hash ace-query)) tns)
    (eval '(set! ace-query-post-hash (make-hash ace-query-post)) tns)
    (cond
     ([file-exists? template-path]
      (set! output (eval `(include-template ,(path->string template-path)) tns)))
     (#t
      (set! response-code 404)
      (set! response-const #"Not found")
      (set! output "<h1>File not found</h1>")))
    (response/full
     response-code response-const
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8 output)))))

;; launch the servlet
(define (ace-server)
  (serve/servlet ace-servlet
                 #:launch-browser? #f
                 #:listen-ip #f
                 #:port 8099
                 #:stateless? #t
                 #:server-root-path "/home/rat/src/ace-server"
                 #:servlets-root "/home/rat/src/ace-server"
                 #:servlet-regexp #rx""))
