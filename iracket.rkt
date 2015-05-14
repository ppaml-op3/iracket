#lang racket/base

(require racket/string
         racket/match
         racket/place
         racket/contract
         (for-syntax racket/base)
         json
         net/zmq
         "./ipython.rkt")

(provide main)

;; implements the ipython heartbeat protocol
(define (heartbeat socket _req _rep)
  (define (loop)
    (printf "heartbeat loop start\n")
    (define msg (socket-recv! socket))
    (printf "heartbeat: ~a\n" msg)
    (socket-send! socket msg)
    (loop))
  (loop))

;; implements shell and control protocol
(define (shell socket req rep)
  (printf "shell thread start\n")
  (define (loop)
    (define msg (receive-message! socket))
    (printf "shell: ~a\n" msg)
    (place-channel-put rep msg)
    (place-channel-put rep req)
    (define response (place-channel-get req))
    (printf "shell response: ~a\n" response)
    (send-message! socket response))
  (loop))

(define control shell)

(define (iopub socket req _rep)
  (printf "iopub thread start\n")
  (define (loop)
    (define msg (place-channel-get req))
    (send-message! socket msg))
  (loop))

(define/contract (config->endpoint cfg kind)
  (-> ipython-config? (symbols 'shell 'control 'iopub 'heartbeat)
      string?)
  (define port
    (case kind
      [(shell) (ipython-config-shell-port cfg)]
      [(control) (ipython-config-control-port cfg)]
      [(iopub) (ipython-config-iopub-port cfg)]
      [(heartbeat) (ipython-config-hb-port cfg)]))
  (define transport (ipython-config-transport cfg))
  (define ip (ipython-config-ip cfg))
  (format "~a://~a:~a" transport ip port))

(define (serve-socket ctx endpoint socket-type action)
  (call-with-socket ctx socket-type
    (lambda (socket)
      (socket-bind! socket endpoint)
      (action socket))))

(define (serve-socket/place ctx endpoint socket-type action)
  (define req
    (place req
      (define ctx (place-channel-get req))
      (define endpoint (place-channel-get req))
      (define socket-type (place-channel-get req))
      (define action (place-channel-get req))
      (serve-socket ctx endpoint socket-type
         (lambda (socket) (action socket req)))))
    (place-channel-put req ctx)
    (place-channel-put req endpoint)
    (place-channel-put req socket-type)
    (place-channel-put req action)
    req)

(define-struct/contract ipython-services
  ([heartbeat place?]
   [shell place?]
   [control place?]
   [iopub place?])
  #:transparent)

(define (ipython-serve cfg rep)
  (call-with-context
   (lambda (ctx)
     (make-ipython-services
      (serve-socket/place ctx (config->endpoint cfg 'heartbeat)
                          'REP rep heartbeat)
      (serve-socket/place ctx (config->endpoint cfg 'shell)
                          'ROUTER rep shell)
      (serve-socket/place ctx (config->endpoint cfg 'control)
                          'ROUTER rep control)
      (serve-socket/place ctx (config->endpoint cfg 'iopub)
                          'PUB rep iopub)))))


(define (main config-file-path)
  (parameterize ([current-output-port (current-error-port)])
    (print "Kernel starting.\n")
    (printf "Kernel config file: ~a" config-file-path)
    ;; TODO check that file exists
    (define cfg (with-input-from-file config-file-path read-ipython-config))
    (printf "Kernel config: ~a" cfg)
    (define-values (req rep)  (place-channel))
    (define services (ipython-serve cfg rep))
    (print "Listener threads started.")
    (work rep services)
    (print "Kernel terminating.")))

(define (work req services)
  (define (loop)
    (define msg (place-channel-get req))
    (define respond-to (place-channel-get req))
    (define response (handle msg req services))
    (place-channel-put respond-to response)
    (loop))
  (loop))

(define (handle msg req services)
  (printf "received message: ~a\n" msg)
  (case (ipython-message-type msg)
    [(kernel-info-request) kernel-info]
    [else (error (format "unknown message type: ~a" (ipython-message-type msg)))]))


(define kernel-info
  (jsexpr->bytes
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
                              'url "http://docs.racket-lang.org")))))
