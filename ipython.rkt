#lang racket/base

(require racket/string
         racket/contract
         racket/match
         (for-syntax racket/base)
         json
         (only-in ffi/unsafe free)
         sha
         net/zmq)

(provide (struct-out ipython-config)
         (struct-out ipython-message)
         read-ipython-config
         receive-message!
         send-message!)

;; IPython's ZeroMQ bindings configuration.
(define-struct/contract ipython-config
  ([control-port exact-nonnegative-integer?]
   [shell-port exact-nonnegative-integer?]
   [transport string?]
   [signature-scheme (symbols 'hmac-sha256)]
   [stdin-port exact-nonnegative-integer?]
   [hb-port exact-nonnegative-integer?]
   [ip string?]
   [iopub-port exact-nonnegative-integer?]
   [key bytes?])
  #:transparent)

(define-struct/contract ipython-message
  ([identifiers (listof bytes?)]
   [parent-header jsexpr?]
   [metadata (hash/c string? string?)]
   [message-id string?] ;; (uuid)
   [session-id string?] ;; (uuid)
   [username string?]
   [type (symbols 'kernel-info-reply
                  'kernel-info-request
                  'execute-reply
                  'execute-request
                  'status
                  'stream
                  'display-data
                  'pyout
                  'pyin
                  'complete-request
                  'complete-reply
                  'object-info-request
                  'object-info-reply
                  'shutdown-request
                  'shutdown-reply
                  'clear-output
                  'input-request
                  'input-reply
                  'comm-open
                  'comm-msg
                  'comm-close
                  'history-request
                  'history-reply)]
   [content jsexpr?])
  #:transparent)


;; Parses an IPython configuration from (current-input-port).
(define (read-ipython-config)
  (define config-json (read-json))
  (apply make-ipython-config
         (map (lambda (x) (hash-ref config-json x))
              '(control_port
                shell_port
                transport
                signature_scheme
                stdin_port
                hb_port
                ip
                iopub_port
                key))))

(define ipython-connection-key (make-parameter #f))

(define (receive-message! socket)
  (printf "receiving message")
  (define (next)
    (define blob (socket-recv! socket))
    (printf "received blob: ~a\n" blob)
    blob)
  (define (read-until str)
    (define (read-until* acc)
      (define blob (next))
      (if (equal? blob str)
          acc
          (read-until* (cons blob acc))))
    (read-until* '()))
  (define idents (read-until message-delimiter))
  (define sig (next))
  (define header-data (next))
  (define parent-header (next))
  (define metadata (next))
  (define content (next))
  (parse-message sig idents header-data parent-header
                 metadata content))

(define (parse-message sig idents header-data parent-header metadata content)
  (define key (ipython-connection-key))
  (unless (or (not key)
              (equal? sig (hash-message key header-data parent-header metadata content)))
    (error "Message from unauthenticated user."))
  (define header-json (bytes->jsexpr header-data))
  (make-ipython-message
   idents
   (bytes->jsexpr parent-header)
   (bytes->jsexpr metadata)
   (hash-ref header-json 'msg_id)
   (hash-ref header-json 'session)
   (hash-ref header-json 'username)
   (string->symbol (string-replace "_" "-" (hash-ref header-json 'msg_type)))
   (bytes->jsexpr content)))

(define (hash-message key header-data parent-header metadata content)
  (define data (bytes-append header-data parent-header metadata content))
  (hmac-sha256 key data))

(define message-delimiter (string->bytes/utf-8 "<IDS|MSG>"))

(define (send-message! socket msg)
  (define (send-piece! data)
    (send! socket data 'SNDMORE))
  (define (send-last! data msg)
    (send! socket data '()))
  (define idents (ipython-message-identifiers msg))
  (define header-data (hasheq
                       'msg_id (ipython-message-message-id msg)
                       'username (ipython-message-username msg)
                       'session (ipython-message-session-id msg)
                       'msg_type (ipython-message-type msg)
                       'version "5.0"))
  (define header (jsexpr->bytes header-data))
  (define parent-header (jsexpr->bytes (ipython-message-parent-header msg)))
  (define metadata (jsexpr->bytes (ipython-message-metadata msg)))
  (define content (jsexpr->bytes (ipython-message-content msg)))
  (define sig (hash-message (ipython-connection-key)
                            header-data parent-header metadata content))
  (printf "sig: ~a\n" sig)
  (for ([ident (in-list idents)]) (send-piece! ident))
  (send-piece! message-delimiter)
  (send-piece! sig)
  (send-piece! header)
  (send-piece! parent-header)
  (send-piece! metadata)
  (send-last! content))

(define (send! socket data flags)
  (define msg (make-msg-with-data data))
  (dynamic-wind
    (void)
    (socket-send-msg! msg socket flags)
    (free msg)))
