#lang racket/base

(require racket/contract
         racket/string)

(provide unknown-message-type/c
         request-message-type/c
         reply-message-type/c
         message-type/c
         mime-type/c
;         request-message/c
;         reply-message/c
;         (struct-out execute-request)
;         execute-reply-status/c
;         (struct-out execute-reply-ok)
;         (struct-out execute-reply-error)
;         (struct-out execute-reply-abort))
         )

(define unknown-message-type/c
  (symbols 'status
           'stream
           'pyout
           'pyin
           'clear_output
           'comm_open
           'comm_msg
           'comm_close))

(define request-message-type/c
  (symbols 'kernel_info_request
           'execute_request
           'complete_request
           'object_info_request
           'shutdown_request
           'input_reply
           'history_request))

(define reply-message-type/c
  (symbols 'kernel_info_reply
           'execute_reply
           'display_data
           'complete_reply
           'object_info_reply
           'shutdown_reply
           'input_request
           'history_reply))

(define message-type/c
  (or/c request-message-type/c
        reply-message-type/c
        unknown-message-type/c))

(define mime-type/c
  (symbols 'application/json
           'application/pdf
           'application/xml
           'image/gif
           'image/jpeg
           'image/png
           'image/bmp
           'image/svg+xml
           'image/tiff
           'text/csv
           'text/html
           'text/markdown
           'text/plain
           'text/rtf
           'text/xml
           'video/avi
           'video/mpeg
           'video/mp4))

; (define request-message/c
;   (or/c execute-request?))
; 
; (define reply-message/c
;   (or/c execute-reply/c))
; 
; (define-struct/contract execute-request
;   ([code string?]
;    [silent boolean?]
;    [store-history boolean?]
;    [user-expressions hash?] ;; XXX what is this?
;    [allow-stdin boolean?]
;    [stop-on-error boolean?])
;   #:transparent)
; 
; (define execute-reply-status/c
;   (symbols 'ok 'error 'abort))
; 
; (define execute-reply/c
;   (or execute-reply-ok?
;       execute-reply-error?
;       execute-reply-abort?))
; 
; (define-struct/contract execute-reply-ok
;   ([execution-count exact-nonnegative-integer?]
;    [user-expressions hash?]) ;; XXX what is this?
;   #:transparent)
; 
; (define-struct/contract execute-reply-error
;   ([execution-count exact-nonnegative-integer?]
;    [ename string?]
;    [evalue string?]
;    [traceback (listof string?)])
;   #:transparent)
; 
; (define-struct/contract execute-reply-abort
;   ([execution-count exact-nonnegative-integer?])
;   #:transparent)
