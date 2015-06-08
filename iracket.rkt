#lang racket/base

(require racket/string
         racket/match
         racket/contract
         racket/sandbox
         racket/port
         setup/dirs
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

(define (send-stream msg iopub stream str)
  (thread-send iopub
               (make-response msg (hasheq 'name stream
                                          'text str)
                              #:msg-type 'stream)))

(define (send-exec-result msg iopub execution-count data)
  (thread-send iopub
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
                   [sandbox-eval-limits (list 30 50)]
                   [sandbox-memory-limit 200]
                   [sandbox-namespace-specs (list sandbox-make-namespace
                                                  'file/convertible)]
                   [sandbox-path-permissions (list (list 'read "/"))])
      (define e
        (parameterize ([sandbox-propagate-exceptions #f])
          (make-evaluator '(begin)
                          #:allow-for-require '(gamble gamble/viz))))

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
      [(complete_request) (values (complete msg services e) #f)]
      [else (error (format "unknown message type: ~a" msg-type))]))
  (values (make-response msg resp) shutdown))

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

(define (read-string-avail)
  (define bstr (bytes))
  (read-bytes-avail! bstr)
  (bytes->string/utf-8 bstr))

(define (make-ipython-stream-port iopub port-name stream-name orig-msg)
  (make-output-port
   port-name
   iopub
   (λ (bstr start end enable-buffer? enable-break?)
     (send-stream orig-msg iopub stream-name
                  (bytes->string/utf-8 (subbytes bstr start end)))
     (- end start))
   void))

(define (execute msg services e)
  (define iopub (ipython-services-iopub services))
  (set! execution-count (add1 execution-count))
  (define code (hash-ref (ipy:message-content msg) 'code))
  (call-in-sandbox-context e
   (λ ()
     (current-output-port (make-ipython-stream-port iopub "pyout" "stdout" msg))
     (current-error-port (make-ipython-stream-port iopub "pyerr" "stderr" msg))))
  (call-with-values
   (λ () (e code))
   (λ vs
     (match vs
       [(list (? void?)) void]
       [else (for ([v (in-list vs)])
               (define results (make-display-results v))
               (send-exec-result msg iopub execution-count
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
