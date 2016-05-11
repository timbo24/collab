#lang racket
(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button
         "client.rkt"
         "parse.rkt")

(provide tool@)

(define in #f)
(define out #f)
(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define collab-mixin
      (mixin ((class->interface text%)) ()
        
        (inherit begin-edit-sequence
                 end-edit-sequence
                 insert
                 get-text
                 undo)

        (thread (lambda ()
                  (msg-listener)))

        (define local (make-parameter #t))
        
        (define/augment (on-insert start len)
          (begin-edit-sequence))
        (define/augment (after-insert start len)
          (let [(end (+ start len))]
            (when (and (not (boolean? out))
                       (local))
              (send-msg (insert-msg start end (get-text start end)) out)
              (undo))
            (end-edit-sequence)))
        
        (define/augment (on-delete start len)
          (begin-edit-sequence))
        (define/augment (after-delete start len)
          (end-edit-sequence))


        (define/private (msg-listener)
          (unless (boolean? in)
            (sync (handle-evt (read-line-evt in)
                  (λ (line)
                    (cond
                      [(eof-object? line) ((close-ports in
                                                        out))]
                      [else
                       (when (and (not (void? (parse-msg line)))
                                  (= (length (parse-msg line)) 3))
                         (let* [(to-insert (parse-msg line))
                                (start (first to-insert))
                                (end (second to-insert))
                                (msg (third to-insert))]
                           (queue-callback (λ () (parameterize [(local #f)]
                                                   (insert msg start end))))))])))))
          (msg-listener))
        (define/private (insert-msg start end msg)
          (format "(insert ~a ~a ~s)" start end msg))
        (super-new)))
    
    (define connect-button-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text)
        (inherit register-toolbar-button)
        
        (let ((btn
               (new switchable-button%
                    (label "MrCollaborate")
                    (callback (λ (button)
                                (set!-values (in out) (connect))
                                (send-msg "(name tim)" out)
                                (send-msg "(file Untitled.rkt)" out)))
                    (parent (get-button-panel))
                    (bitmap reverse-content-bitmap))))
          (register-toolbar-button btn #:number 11)
          (send (get-button-panel) change-children
                (λ (l)
                  (cons btn (remq btn l)))))))
    
    (define reverse-content-bitmap
      (let* ((bmp (make-bitmap 16 16))
             (bdc (make-object bitmap-dc% bmp)))
        (send bdc erase)
        (send bdc set-smoothing 'smoothed)
        (send bdc set-pen "black" 1 'transparent)
        (send bdc set-brush "blue" 'solid)
        (send bdc draw-ellipse 2 2 8 8)
        (send bdc set-brush "red" 'solid)
        (send bdc draw-ellipse 6 6 8 8)
        (send bdc set-bitmap #f) 
        bmp))
    
    (define (phase1) (void))
    (define (phase2) (void))
    
    (drracket:get/extend:extend-unit-frame connect-button-mixin)
    (drracket:get/extend:extend-definitions-text collab-mixin)))

