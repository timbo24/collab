#lang racket

(provide parse-msg)

(define (parse-msg n)
  (let [(sp (open-input-string n))]
    (match (read sp)
      [`(insert ,start ,end ,msg) `(,start ,end ,msg)]
      [_ (displayln "incorrect")])))