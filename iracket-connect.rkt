#lang racket/base

(require racket/contract
         json
         (prefix-in ipy: "./ipython.rkt"))

(provide connect)

;; connect_request
(define/contract (connect cfg)
  (ipy:config? . -> . jsexpr?)
  (hasheq
   'shell_port (ipy:config-shell-port cfg)
   'iopub_port (ipy:config-iopub-port cfg)
   'stdin_port (ipy:config-stdin-port cfg)
   'hb_port (ipy:config-hb-port cfg)))
