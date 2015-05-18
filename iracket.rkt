#lang racket/base

(require racket/string
         racket/match
         racket/contract
         (for-syntax racket/base)
         json
         net/zmq
         libuuid
         (prefix-in ipy: "./ipython.rkt"))

(provide main)

;; implements the ipython heartbeat protocol
(define (heartbeat socket _worker)
  (define (loop)
    (define msg (socket-recv! socket))
    (socket-send! socket msg)
    (loop))
  (loop))

(define (reply-type parent-type)
  (case parent-type
    [(kernel_info_request) 'kernel_info_reply]
    [(execute_request) 'execute_reply]
    [(complete_request) 'complete_reply]
    [(object_info_request) 'object_info_reply]
    [(shutdown_request) 'shutdown_reply]
    [(history_request) 'history_reply]
    [else (error (format "No reply for message type: ~a" parent-type))]))

(define (create-reply-header parent-header)
  (ipy:make-header
   (ipy:header-identifiers parent-header)
   parent-header
   (make-hasheq)
   (uuid-generate)
   (ipy:header-session-id parent-header)
   (ipy:header-username parent-header)
   (reply-type (ipy:header-msg-type parent-header))))

;; implements shell and control protocol
(define (shell-like who socket worker)
  (sleep 5)
  (define (loop)
    (define msg (ipy:receive-message! socket))
    (printf "~a: ~a\n" who msg)
    (thread-send worker msg)
    (thread-send worker (current-thread))
    (define response (thread-receive))
    ; (define response-msg (make-response-msg response msg))
    (define reply-header (create-reply-header (ipy:message-header msg)))
    (define response-message (ipy:make-message reply-header response))
    (printf "~a response: ~a\n" who response-message)
    (ipy:send-message! socket response-message))
  (loop))

(define (shell socket worker) (shell-like 'shell socket worker))

(define (control socket worker) (shell-like 'control socket worker))

(define (iopub socket worker)
  (define (loop)
    (define msg (thread-receive))
    (printf "iopub thread sending: ~a\n" msg)
    (ipy:send-message! socket msg))
  (loop))

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


(define (main config-file-path)
  (parameterize ([current-output-port (current-error-port)])
    (print "Kernel starting.\n")
    ;; TODO check that file exists
    (define cfg (with-input-from-file config-file-path ipy:read-config))
    (parameterize ([ipy:connection-key (ipy:config-key cfg)])
      (call-with-context
       (λ (ctx)
         (define services (ipython-serve cfg ctx (current-thread)))
         (work services)
         (print "Kernel terminating."))))))

(define (work services)
  (define (work-loop)
    (define msg (thread-receive))
    (printf "work received: ~a" msg)
    (define respond-to (thread-receive))
    (define response (handle msg services))
    (printf "work response: ~a" response)
    (thread-send respond-to response)
    (work-loop))
  (work-loop))

(define (handle msg services)
  (define msg-type (ipy:header-msg-type (ipy:message-header msg)))
  (case msg-type
    [(kernel_info_request) kernel-info]
    [else (error (format "unknown message type: ~a" msg-type))]))

(define kernel-info
  (hasheq
   'protocol_version "0.0.1"
   'implementation "iracket"
   'implementation_version "0.0.1"
   'language_info (hasheq
                   'name "racket"
                   'version (version)
                   'mimetype "text"
                   'file_extension "rkt")
   'banner "IRacket 0.0.1"
   'help_links (list (hasheq 'text "Racket docs"
                             'url "http://docs.racket-lang.org"))))
