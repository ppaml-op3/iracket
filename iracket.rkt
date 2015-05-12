#lang racket/base

(require racket/string
         racket/match
         racket/place
         (for-syntax racket/base)
         json
         ffi/unsafe
         net/zmq
         "./ipython.rkt")

(provide main)

;; implements the ipython heartbeat protocol
(define (heartbeat socket _)
  (define (loop)
    (printf "heartbeat loop start\n")
    (define msg (socket-recv! socket))
    (printf "heartbeat: ~a\n" msg)
    (socket-send! socket msg)
    (loop))
  (loop))

;; implements shell and control protocol
(define (shell socket worker-thread)
  (printf "shell thread start\n")
  (define (loop)
    (define msg (receive-message! socket))
    (printf "shell: ~a\n" msg)
    (thread-send worker-thread msg)
    (thread-send (current-thread))
    (define response (thread-receive))
    (printf "shell response: ~a\n" response)
    (send-message! socket response))
  (loop))

(define control shell)

(define (iopub socket _)
  (printf "iopub thread start\n")
  (define (loop)
    (define msg (thread-receive))
    (send-message! socket msg))
  (loop))

(define (serve-socket cfg ctx socket-type port action)
  (define transport (ipython-config-transport cfg))
  (define ip (ipython-config-ip cfg))
  (define endpoint (format "~a://~a:~a" transport ip port))
  (call-with-socket ctx socket-type
    (lambda (socket)
      (socket-bind! socket endpoint)
      (action socket))))

;; Starts the services in their own threads, except for the first in the list
;; which is started in the current thread.
(define (serve cfg ctx worker-thread services)
  (for/list ([service (in-list (cdr services))])
    (match service
      [`(,socket-type ,port ,action)
       (thread
        (lambda ()
          (serve-socket cfg ctx socket-type (port cfg)
                        (lambda (socket) (action socket worker-thread)))))]))
  (match-define `(,socket-type ,port ,action) (car services))
  (serve-socket cfg ctx socket-type (port cfg)
                (lambda (socket) (action socket worker-thread))))

;; Starts the IPython services in their own threads,
;; except for the iopub service, which is run in the current thread.
(define (ipython-serve cfg worker-thread)
  (define services
    `((PUB ,ipython-config-iopub-port ,iopub)
      (REP ,ipython-config-hb-port ,heartbeat)
      (ROUTER ,ipython-config-control-port ,control)
      (ROUTER ,ipython-config-shell-port ,shell)))
  (call-with-context
   (lambda (ctx)
     (serve cfg ctx worker-thread services))
   #:io-threads 4))

(define (main config-file-path)
  (parameterize ([current-output-port (current-error-port)])
    (print "Kernel starting.\n")
    (printf "Kernel config file: ~a" config-file-path)
    ;; TODO check that file exists
    (define cfg (with-input-from-file config-file-path read-ipython-config))
    (printf "Kernel config: ~a" cfg)
    (define my-thread (current-thread))
    (define iopub-thread (thread (lambda () (ipython-serve cfg my-thread))))
    (print "Listener threads started.")
    (work iopub-thread)
    (print "Kernel terminating.")))

(define (work iopub-thread)
  (define (loop)
    (define msg (thread-receive))
    (define respond-to (thread-receive))
    (define response (handle msg iopub-thread))
    (thread-send respond-to response)
    (loop))
  (loop))

(define (handle msg iopub-thread)
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
