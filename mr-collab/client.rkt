#lang racket

(require racket/tcp)

(provide connect
         close-ports
         send-msg)

(define (connect)
  (tcp-connect "127.0.0.1"
               8080))
(define (close-ports in out)
  (close-output-port out)
  (close-input-port in))

(define (send-msg msg out)
  (displayln msg out)
  (flush-output out))
