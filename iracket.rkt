#lang racket/base

(require racket/string
         racket/match
         racket/contract
         racket/sandbox
         setup/dirs
         pict
         file/convertible
         (for-syntax racket/base)
         json
         net/base64
         (prefix-in ipy: "./ipython-message.rkt")
         (prefix-in ipy: "./ipython-services.rkt")
         (prefix-in ipy: "./ipython.rkt"))

(provide main)

(define (main config-file-path)
  ;; ipython hides stdout, but prints stderr, so this is for debugging
  (current-output-port (current-error-port))
  (display "Kernel starting.\n")
  (define cfg (with-input-from-file config-file-path ipy:read-config))
  (parameterize ([ipy:connection-key (ipy:config-key cfg)]
                 [sandbox-eval-limits (list 30 50)]
                 [sandbox-memory-limit 200]
                 [sandbox-namespace-specs (list sandbox-make-namespace
                                                'file/convertible)]
                 [sandbox-propagate-exceptions #f]
                 [sandbox-path-permissions (list (list 'read "/"))])
    (define e (make-evaluator '(begin) #:allow-for-require '(gamble gamble/viz)))
    (ipy:call-with-services cfg (λ (services) (work cfg services e))))
  (display "Kernel terminating.\n"))

(define (work cfg services e)
  (let loop ()
    (define-values (msg respond-to) (ipy:receive-request services))
    (ipy:send-status services (ipy:message-header msg) 'busy)
    (define-values (response shutdown) (handle msg cfg services e))
    (ipy:send-response services respond-to response)
    (ipy:send-status services (ipy:message-header msg) 'idle)
    (unless shutdown (loop))))

(define (handle msg cfg services e)
  (define msg-type (ipy:header-msg-type (ipy:message-header msg)))
  (define-values (resp shutdown)
    (case msg-type
      [(kernel_info_request) (values kernel-info #f)]
      [(shutdown_request) (values (hasheq 'restart #f) #t)]
      [(connect_request) (values (connect cfg) #f)]
      [(execute_request) (values (execute msg services e) #f)]
      [(complete_request) (values (complete msg services e) #f)]
      [else (error (format "unknown message type: ~a" msg-type))]))
  (values (ipy:make-response msg resp) shutdown))

(define/contract (string-prefix? prefix word)
  (string? string? . -> . boolean?)
  (define len (string-length prefix))
  (equal? prefix (substring word 0 (min (string-length word) len))))

(define (complete msg services e)
  (define code (hash-ref (ipy:message-content msg) 'code))
  (define cursor-pos (hash-ref (ipy:message-content msg) 'cursor_pos))
  (define prefix (car (regexp-match #px"[^\\s,)(]*$" code 0 cursor-pos)))
  (define suffix (car (regexp-match #px"^[^\\s,)(]*" code (sub1 cursor-pos))))
  (define words (call-in-sandbox-context e namespace-mapped-symbols))
  (define matches
    (sort (filter (λ (w) (string-prefix? prefix w))
                  (map symbol->string words))
          string<=?))
  (hasheq
   'matches matches
   'cursor_start (- cursor-pos (string-length prefix))
   'cursor_end (+ cursor-pos (string-length suffix) -1)
   'status "ok"))


(define (connect cfg)
  (hasheq
   'shell_port (ipy:config-shell-port cfg)
   'iopub_port (ipy:config-iopub-port cfg)
   'stdin_port (ipy:config-stdin-port cfg)
   'hb_port (ipy:config-hb-port cfg)))

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
                (make-display-convertible 'svg-bytes 'image/svg+xml v)
                (make-display-convertible 'png-bytes 'image/png v #:encode base64-encode)
                (make-display-convertible 'gif-bytes 'image/gif v #:encode base64-encode)
                (make-display-convertible 'ps-bytes 'application/postscript v #:encode base64-encode)
                (make-display-convertible 'pdf-bytes 'application/pdf v #:encode base64-encode))))

(define execution-count 0)

(define (execute msg services e)
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
   'user_expressions (hasheq)))

(define doc-url
  (match (find-doc-dir)
    [#f  "http://docs.racket-lang.org"]
    [d (string-append "file://" (path->string (build-path d "index.html")))]))

(define kernel-info
  (hasheq
   'language_info (hasheq
                   'mimetype "text/x-racket"
                   'name "Racket"
                   'version (version)
                   'file_extension ".rkt"
                   'pygments_lexer "racket"
                   'codemirror_mode "scheme")

   'implementation "iracket"
   'implementation_version "1.0"
   'protocol_version "5.0"

   'language "Racket"

   'banner "IRacket 1.0"
   'help_links (list (hasheq
                      'text "Racket docs"
                      'url doc-url))))
