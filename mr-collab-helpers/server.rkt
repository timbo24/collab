#lang racket

(require racket/tcp
         lens
         racket/class
         racket/gui/base)

;; Setting up lenses and structs
(struct/lens user (in out))
(struct/lens file (name users))
;; Composing lens for user hash-table updates
(define (set-user-lens user)
  (lens-compose (hash-ref-lens user)
                file-users-lens))

(define dir (current-directory))

;; list of all files in dir
(define file-names
  ;; filter out all but .rkt files 
  ;; filter out any dirs
  (filter (λ (path)
            (and (file-exists? path)
                 (string-suffix? (path-element->string path)
                                 ".rkt")))
          ;; get all files/directories 
          (directory-list dir)))

(define (send-msg msg out)
  (displayln msg out)
  (flush-output out))

(define (broadcast-msg msg users)
  (hash-for-each users
                 (λ (name user)
                   (send-msg msg (user-out user)))))

(define (parse-msg n users)
  (let [(sp (open-input-string n))]
    (match (read sp)
      [`(insert ,start ,end ,msg) (broadcast-msg n users)]
      [_ (displayln "incorrect")])))

;; Server starts with path, unitialize a file struct with path, text%, and empty
;; user hash-table
(define (make-server path)
  (define add-user-ch (make-channel))
  
  (define (server f)
    (apply sync
           (handle-evt add-user-ch
                       (λ (name-user)
                         (let [(name (car name-user))
                               (user (cdr name-user))]
                           (server (lens-set (set-user-lens name) f user)))))
           (message-received-evts f)))
  
  ;; spawn list of event handlers for read-line-evts for each user
  (define (message-received-evts f)
    (hash-map (file-users f)
              (λ (name user)
                (handle-evt (read-line-evt (user-in user))
                            (λ (line)
                              (cond
                                [(eof-object? line) ((close-ports (user-in user)
                                                                  (user-out user))
                                                     (server (lens-transform file-users-lens
                                                                             f
                                                                             (λ (users)
                                                                               (hash-remove users name)))))]
                                [else (printf "message received from ~a: ~a ~n" name line)
                                  (parse-msg line (file-users f))])
                              ;; TODO
                              (server f))))))
  
  ;; generate list of event handlers for eof-evts for each user
  #;(define (eof-evts f)
    (hash-map (file-users f)
              (λ (name user)
                (handle-evt (eof-evt (user-in user))
                            (λ (eof)
                              (close-ports (user-in user)
                                           (user-out user))
                              (server (lens-transform file-users-lens
                                                     f
                                                     (λ (users)
                                                       (hash-remove users name)))))))))
  
  
  (thread (λ () (server (file path (make-immutable-hash)))))
  
  add-user-ch)

;; Create an immutable hash-table mapping name of files to the
;; add-user-channel
(define files
  (make-immutable-hash
   (map (λ (path)
          (cons (path-element->string path) (make-server path)))
        file-names)))

(define (is-username? n)
  (let [(sp (open-input-string n))]
    (match (read sp)
      [`(name ,n) #t]
      [_ #f])))

(define (get-username n)
  (let [(sp (open-input-string n))]
    (match (read sp)
      [`(name ,n) (symbol->string n)])))

(define (is-filename? n)
  (let [(sp (open-input-string n))]
    (match (read sp)
      [`(file ,n) #t]
      [_ #f])))

(define (get-filename n)
  (let [(sp (open-input-string n))]
    (match (read sp)
      [`(file ,n) (symbol->string n)])))

(define (close-ports in out)
  (close-input-port in)
  (close-output-port out))

(define (create-file-list-msg fns)
  (apply string-append
         "(files "
         (append fns '(")"))))

;; listening for connections
(define (listening-server lnr)
  (let-values ([(in out) (tcp-accept lnr)])
    (define (wait-for-username)
      (let [(line (read-line in))]
        (if (is-username? line)
            (let [(username (get-username line))]
              #;(send-msg (create-file-list-msg (map path-element->string
                                                   file-names)) out)
              (wait-for-filename username))
            (close-ports in out))))
    (define (wait-for-filename username)
      (let [(line (read-line in))]
        (if (is-filename? line)
            (let [(filename (get-filename line))]
              (channel-put (hash-ref files filename)
                           (cons username (user in out))))
            (close-ports in out))))
    (thread (λ ()
              (wait-for-username)))
    (listening-server lnr)))



(define listener (tcp-listen 8080 4 #f "127.0.0.1"))
(listening-server listener)
