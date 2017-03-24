#lang web-server

(require web-server/servlet-env)

(provide (rename-out [ace-start ace]))

(define hosts #hash{["localhost" . #hash{}]})

(define (ace-server req)
  (let ([tns (make-base-namespace)]
        [hostname (hash-ref (make-hash (request-headers req)) 'host)]
        [uri (regexp-replace #rx"\\?.*$" (url->string (request-uri req)) "")]
        [query (url-query (request-uri req))]
        [body null]
        [filename null]
        [filepath null]
        [base-path "/www"]
        [content-type TEXT/HTML-MIME-TYPE]
        [settings null]
        [http-code (list 200 #"OK")]
        [query-hash null])
    (for ([hostre (hash-keys hosts)])
      (let ([hre (if (regexp? hostre) hostre (regexp hostre))])
        (when (regexp-match hre hostname)
          (set! settings (hash-ref hosts hostre)))))
    (when (hash? settings)
      (set! base-path (hash-ref settings 'base-path))
      (eval '(require web-server/templates) tns)
      (for ([mod (hash-ref settings 'modules '())])
        (eval `(require ,mod) tns))
      (eval `(current-directory ,base-path) tns)
      (set! query-hash (make-hash query))
      (namespace-set-variable-value! 'request req #f tns)
      (namespace-set-variable-value! 'query query-hash #t tns)
      (set! filename (string-join (list base-path uri) ""))
      (set! body
            (with-handlers
                ([exn:fail:filesystem? (lambda (v)
                                   (set! http-code (list 404 #"Not Found"))
                                   (set! content-type #"text/plain")
                                   (format "~a" (exn-message v)))]
                 [exn:fail? (lambda (v)
                              (set! http-code (list 500 #"Server Error"))
                              (set! content-type #"text/plain")
                              (format "exception:\n~a" v))])
              (set! filepath (build-path filename))
              (set! filepath (find-relative-path (current-directory) filepath))
              (unless (file-exists? filepath)
                (raise (make-exn:fail:filesystem (format "File not found: ~a" uri)
                                                 (current-continuation-marks))))
              (eval `(include-template ,(path->string filepath)) tns)))
      (set! body (list (string->bytes/utf-8 body)))
      (response/full (first http-code)
                     (second http-code)
                     (current-seconds)
                     content-type
                     null
                     body))))

(define (ace-start
         #:hosts [ace-hosts null]
         #:port  [port 8086])
  (when (hash? ace-hosts)
    (set! hosts ace-hosts))
  (serve/servlet ace-server
                 #:launch-browser? #f
                 #:servlet-regexp #px""
                 #:port port))
