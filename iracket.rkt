#lang racket/base

(require racket/string
         racket/match
         racket/contract
         racket/sandbox
         pict
         file/convertible
         (for-syntax racket/base)
         json
         net/base64
         net/zmq
         libuuid
         (prefix-in ipy: "./ipython.rkt"))

(provide main)

;; implements the ipython heartbeat protocol
(define (heartbeat socket _worker)
  (let loop ()
    (define msg (socket-recv! socket))
    (socket-send! socket msg)
    (loop)))

(define (make-response parent content #:msg-type [msg-type #f])
  (define reply-header
    (make-reply-header (ipy:message-header parent) #:msg-type msg-type))
  (ipy:make-message reply-header content))

(define (reply-type parent-type)
  (case parent-type
    [(kernel_info_request) 'kernel_info_reply]
    [(execute_request) 'execute_reply]
    [(complete_request) 'complete_reply]
    [(object_info_request) 'object_info_reply]
    [(shutdown_request) 'shutdown_reply]
    [(history_request) 'history_reply]
    [else (error (format "No reply for message type: ~a" parent-type))]))

(define (make-reply-header parent-header #:msg-type [msg-type #f])
  (ipy:make-header
   (ipy:header-identifiers parent-header)
   parent-header
   (make-hasheq)
   (uuid-generate)
   (ipy:header-session-id parent-header)
   (ipy:header-username parent-header)
   (if msg-type msg-type (reply-type (ipy:header-msg-type parent-header)))))

;; implements shell and control protocol
(define (shell-like who socket worker)
  (let loop ()
    (define msg (ipy:receive-message! socket))
    (printf "~a: ~a\n" who msg)
    (thread-send worker msg)
    (thread-send worker (current-thread))
    (define response (thread-receive))
    (printf "~a response: ~a\n" who response)
    (ipy:send-message! socket response)
    (loop)))

(define (shell socket worker) (shell-like 'shell socket worker))

(define (control socket worker) (shell-like 'control socket worker))

(define (iopub socket worker)
  (let loop ()
    (define msg (thread-receive))
    (printf "iopub thread sending: ~a\n" msg)
    (ipy:send-message! socket msg)
    (loop)))

(define (serve-socket ctx endpoint socket-type action)
  (call-with-socket ctx socket-type
    (lambda (socket)
      (socket-bind! socket endpoint)
      (action socket))))

(define-struct/contract ipython-services
  ([heartbeat thread?]
   [shell thread?]
   [control thread?]
   [iopub thread?])
  #:transparent)

(define (serve-socket/thread ctx cfg port socket-type worker action)
  (define transport (ipy:config-transport cfg))
  (define ip (ipy:config-ip cfg))
  (define endpoint (format "~a://~a:~a" transport ip port))
  (thread
   (λ () (serve-socket ctx endpoint socket-type
                       (λ (socket) (action socket worker))))))

(define (ipython-serve cfg ctx worker)
  (make-ipython-services
   (serve-socket/thread ctx cfg (ipy:config-hb-port cfg) 'REP worker heartbeat)
   (serve-socket/thread ctx cfg (ipy:config-shell-port cfg) 'ROUTER worker shell)
   (serve-socket/thread ctx cfg (ipy:config-control-port cfg) 'ROUTER worker control)
   (serve-socket/thread ctx cfg (ipy:config-iopub-port cfg) 'PUB worker iopub)))

(define (kill-services services)
  (kill-thread (ipython-services-shell services))
  (kill-thread (ipython-services-control services))
  (kill-thread (ipython-services-iopub services))
  (kill-thread (ipython-services-heartbeat services)))

(define (send-stream msg services stream str)
  (thread-send (ipython-services-iopub services)
               (make-response msg (hasheq 'name stream
                                          'text str)
                              #:msg-type 'stream)))

(define (send-exec-result msg services execution-count data)
  (thread-send (ipython-services-iopub services)
               (make-response msg (hasheq 'execution_count execution-count
                                          'data data
                                          'metadata (hasheq))
                              #:msg-type 'execute_result)))

(define execution-count 0)

(define (main config-file-path)
  (parameterize ([current-output-port (current-error-port)])
    (print "Kernel starting.\n")
    ;; TODO check that file exists
    (define cfg (with-input-from-file config-file-path ipy:read-config))
    (parameterize ([ipy:connection-key (ipy:config-key cfg)]
                   [sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-propagate-exceptions #f]
                   [sandbox-eval-limits (list 30 50)]
                   [sandbox-memory-limit 200]
                   [sandbox-namespace-specs (list sandbox-make-namespace
                                                  'file/convertible)]
                   [sandbox-path-permissions (list (list 'read "/"))])
      (define e (make-evaluator 'gamble
                                #:allow-for-require '(gamble/viz)))

      (call-with-context
       (λ (ctx)
         (define services (ipython-serve cfg ctx (current-thread)))
         (work cfg services e)
         (sleep 1)
         (kill-services services)
         (print "Kernel terminating."))))))

(define (send-status status parent-header services)
  (define out (ipython-services-iopub services))
  (define header (make-reply-header parent-header #:msg-type 'status))
  (define msg (ipy:make-message header (hasheq 'execution_state (symbol->string status))))
  (thread-send out msg))

(define (work cfg services e)
  (define (work-loop)
    (define msg (thread-receive))
    (define respond-to (thread-receive))
    (send-status 'busy (ipy:message-header msg) services)
    (define-values (response shutdown) (handle msg cfg services e))
    (thread-send respond-to response)
    (send-status 'idle (ipy:message-header msg) services)
    (unless shutdown (work-loop)))
  (work-loop))

(define (handle msg cfg services e)
  (define msg-type (ipy:header-msg-type (ipy:message-header msg)))
  (define-values (resp shutdown)
    (case msg-type
      [(kernel_info_request) (values kernel-info #f)]
      [(shutdown_request) (values (hasheq 'restart #f) #t)]
      [(connect_request) (values (connect cfg) #f)]
      [(execute_request) (values (execute msg services e) #f)]
      [else (error (format "unknown message type: ~a" msg-type))]))
  (values (make-response msg resp) shutdown))

(define (connect cfg)
  (hasheq
   'shell_port (ipy:config-shell-port cfg)
   'iopub_port (ipy:config-iopub-port cfg)
   'stdin_port (ipy:config-stdin-port cfg)
   'hb_port (ipy:config-hb-port cfg)))

(define (make-display-text vs)
  (define v (match vs
              [(list v) v]
              [else vs]))
  (if (void? v)
      '()
      (list (cons 'text/plain (format "~a" v)))))

(define (make-display-png vs)
  (define png (match vs
                [(list (? convertible? p)) (convert p 'png-bytes)]
                [else #f]))
  (if png
      (list (cons 'image/png (bytes->string/latin-1 (base64-encode png))))
      '()))

(define (execute msg services e)
  (set! execution-count (add1 execution-count))
  (call-with-values
   (λ () (e (hash-ref (ipy:message-content msg) 'code)))
   (λ v
     (define results (append (make-display-text v)
                                (make-display-png v)))
     (unless (null? results)
     (send-exec-result msg services execution-count
                       (make-hasheq results)))))
  (send-stream msg services "stdout" (get-output e))
  (send-stream msg services "stderr" (get-error-output e))
  (hasheq
   'status "ok"
   'execution_count execution-count
   'user_expressions (hasheq)))

(define kernel-info
  (hasheq
   'language_info (hasheq
                   'mimetype "text/x-racket"
                   'name "racket"
                   'pygments_lexer "racket"
                   'version (version)
                   'file_extension ".rkt"
                   'codemirror_mode "Scheme"
                   'pygments_lexer "Scheme")

   'implementation "iracket"
   'implementation_version "1.0"
   'protocol_version "5.0"

   'banner "IRacket 1.0"
   'help_links (list (hasheq
                      'text "Racket docs"
                      'url "http://docs.racket-lang.org"))))
