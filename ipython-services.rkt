#lang racket/base

(require racket/string
         racket/match
         racket/contract
         racket/port
         (for-syntax racket/base)
         net/zmq
         (prefix-in ipy: "./ipython.rkt"))

(provide call-with-services
         services
         (struct-out ipython-services))

(define-struct/contract ipython-services
  ([heartbeat thread?]
   [shell thread?]
   [control thread?]
   [iopub thread?])
  #:transparent)

;; implements the ipython heartbeat protocol
(define (heartbeat socket _worker)
  (let loop ()
    (define msg (socket-recv! socket))
    (socket-send! socket msg)
    (loop)))

;; implements shell and control protocol
(define (shell-like who socket worker)
  (let loop ()
    (define msg (ipy:receive-message! socket))
    (printf "~a received: ~a\n" who (ipy:header-msg-type (ipy:message-header msg)))
    (thread-send worker msg)
    (thread-send worker (current-thread))
    (define response (thread-receive))
    (printf "~a response: ~a\n" who (ipy:header-msg-type (ipy:message-header response)))
    (ipy:send-message! socket response)
    (loop)))

(define (shell socket worker) (shell-like 'shell socket worker))

(define (control socket worker) (shell-like 'control socket worker))

(define (iopub socket worker)
  (let loop ()
    (define msg (thread-receive))
    (printf "iopub thread sending: ~a\n" (ipy:header-msg-type (ipy:message-header msg)))
    (ipy:send-message! socket msg)
    (loop)))

(define (serve-socket ctx endpoint socket-type action)
  (call-with-socket ctx socket-type
    (lambda (socket)
      (socket-bind! socket endpoint)
      (action socket))))

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

(define services (make-parameter #f ipython-services?))

(define heartbeat-thread (make-parameter #f thread?))
(define shell-thread (make-parameter #f thread?))
(define control-thread (make-parameter #f thread?))
(define iopub-thread (make-parameter #f thread?))

(define (call-with-services cfg action)
  (call-with-context
   (λ (ctx)
     (define worker (current-thread))
     (define (serve port port-type thunk)
       (serve-socket/thread ctx cfg (port cfg) port-type worker thunk))
     (define services
       (make-ipython-services
        (serve ipy:config-hb-port 'REP heartbeat)
        (serve ipy:config-shell-port 'ROUTER shell)
        (serve ipy:config-control-port 'ROUTER control)
        (serve ipy:config-iopub-port 'PUB iopub)))
       (begin0
           (action services)
           (kill-services (services))))))
