#lang racket/base

(provide kernel-info)

;; kernel_info_request
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
                      'url "http://docs.racket-lang.org")
                     (hasheq
                      'text "Gamble docs"
                      'url "http://rmculpepper.github.io/gamble/"))))
