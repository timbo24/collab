#lang racket

(require racket/class
         racket/gui/base
         drracket/tool
         racket/unit)

(struct file (path users))
(define an-editor (new text%))

(define dir (current-directory))

(define files
  ;; filter out all but .rkt files 
  ;; filter out any dirs
  (filter (λ (path)
            (and (file-exists? path)
                 (string-suffix? (path-element->string path)
                                 "test.rkt")))
          ;; get all files/directories 
          (directory-list dir)))

;(send an-editor load-file (first files) 'guess #t)


(send an-editor load-file (first files) 'guess #t)

(send an-editor delete 0 1000 #t)

(send an-editor get-text)
(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define (phase1) (void))
    (define (phase2) (void))
    (displayln (drracket:get/extend:get-definitions-text))))