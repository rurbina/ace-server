#lang web-server

(require web-server/servlet-env)

(provide (rename-out [ace-start ace]))

(define hosts #hash{["localhost" . #hash{}]})

(define (load-doc filename
                  #:base-path    [base-path "."]
                  #:headers-only [nobody #f])
  (let ([body "<empty>"]
        [headers null]
        [slurp ""]
        [filepath (string-join (list base-path filename) "")])
    (if (file-exists? filepath)
        (begin
          (set! slurp (file->string filepath))
          (set! body slurp))
        (begin
          (set! body (string-join (list "noslurp:" filepath)))))
    (values headers body)))

(define (ace-server req)
  (let ([tns (make-base-namespace)]
        [hostname (hash-ref (make-hash (request-headers req)) 'host)]
        [uri (url->string (request-uri req))]
        [headers (make-hash (request-headers req))]
        [body null]
        [filename null]
        [filepath null]
        [base-path "/www"]
        [settings null])
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
      (namespace-set-variable-value! 'request req #f tns)
      (namespace-set-variable-value! 'headers headers #f tns)
      (set! filename (string-join (list base-path uri) ""))
      (set! body
            (with-handlers
                ([exn:fail:contract:divide-by-zero? (lambda (v) "divide-by-zero")]
                 [exn:fail? (lambda (v) (format "caught exception:~v" v))])
              (set! filepath (build-path filename))
              (set! filepath (find-relative-path (current-directory) filepath))
              (eval `(include-template ,(path->string filepath)) tns)))
      (set! body (list (string->bytes/utf-8 body)))
      (response/full 200 #"OK"
                     (current-seconds)
                     TEXT/HTML-MIME-TYPE
                     headers
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
