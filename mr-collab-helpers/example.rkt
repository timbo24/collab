#lang racket
(require racket/class
         racket/gui/base)

(define t (new text%))


(begin
   (send t set-max-undo-history 'forever)
   (send t insert "a")
   (send t insert "b")
   (send t begin-edit-sequence #f #f)
   (send t insert "c" 1 1)
   (send t end-edit-sequence)
   (send t get-text))