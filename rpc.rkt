#lang racket

(require json)

(provide rpc-process)

(define (rpc-process data fn-key)
  (let [(jsreq (when (string? data) (string->jsexpr data)))
        (result null)]
  (jsexpr->string
   (cond
    [(not (jsexpr? data))
     (make-hasheq '([error . "POST data not in valid JSON format"]))]
    [(not (hash? jsreq))
     (make-hasheq '([error . "Bad JSON"]))]
    [(not (hash-has-key? jsreq 'procedure))
     (make-hasheq '([error . "No procedure given"]))]
    [(not (hash-has-key? fn-key (string->symbol (hash-ref jsreq 'procedure))))
     (make-hasheq '((error . (string-append "Procedure not found: " (hash-ref jsreq 'procedure)))))]
    [#t
     (set! result ((hash-ref fn-key (string->symbol (hash-ref jsreq 'procedure))) jsreq))
     (set! result
           (make-hasheq `((result . ,result))))
     result]))))
