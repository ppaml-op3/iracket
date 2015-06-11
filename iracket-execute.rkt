#lang racket/base

(require racket/string
         racket/match
         racket/contract
         racket/sandbox
         file/convertible
         (for-syntax racket/base)
         json
         net/base64
         (prefix-in ipy: "./ipython-message.rkt")
         (prefix-in ipy: "./ipython-services.rkt"))

(provide make-execute)


;; execute_request
(define (make-display-text v)
  (cons 'text/plain (format "~a" v)))

(define (make-display-convertible conversion-type mime-type v
                                  #:encode [encode values])
  (define result (and (convertible? v)
                   (convert v conversion-type)))
  (if result
      (cons mime-type (bytes->string/latin-1 (encode result)))
      #f))

(define (make-display-results v)
  (filter values
          (list (or (make-display-convertible 'text 'text/plain v)
                    (make-display-text v))
                ; svg seems to be broken in tons of browsers
                ; (make-display-convertible 'svg-bytes 'image/svg+xml v)
                (make-display-convertible 'png-bytes 'image/png v #:encode base64-encode)
                (make-display-convertible 'gif-bytes 'image/gif v #:encode base64-encode)
                (make-display-convertible 'ps-bytes 'application/postscript v #:encode base64-encode)
                (make-display-convertible 'pdf-bytes 'application/pdf v #:encode base64-encode))))

(define (make-kill-thread/custodian cust)
  (λ (t)
    (parameterize ([current-custodian cust])
      (kill-thread t))))

(define (make-execute services e)
  (define execution-count 0)
  (define user-cust (get-user-custodian e))
  ;; let other cells' threads be killed
  (call-in-sandbox-context e
   (λ ()
     (eval
      `(define notebook-kill-thread
         ,(make-kill-thread/custodian user-cust)))))
  (λ (msg)
    (set! execution-count (add1 execution-count))
    (define code (hash-ref (ipy:message-content msg) 'code))
    (call-in-sandbox-context e
     (λ ()
       (current-output-port (ipy:make-stream-port services 'stdout msg))
       (current-error-port (ipy:make-stream-port services 'stderr msg))))
    (call-with-values
     (λ () (e code))
     (λ vs
       (match vs
         [(list (? void?)) void]
         [else (for ([v (in-list vs)])
                 (define results (make-display-results v))
                 (ipy:send-exec-result msg services execution-count
                                       (make-hasheq results)))])))
    (hasheq
     'status "ok"
     'execution_count execution-count
     'user_expressions (hasheq))))
