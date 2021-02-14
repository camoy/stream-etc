#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [stream-group-by (->* (stream? (-> any/c any))
                        ((-> any/c any/c boolean?))
                        (listof list?))])
 (rename-out [λ-stream~> lambda-stream~>])
 stream~>
 λ-stream~>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/list
         racket/stream)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations

;; Stolen from `racket/list`.
(define (stream-group-by l key [=? equal?])
  (define (alist-update al k up fail)
    (let loop ([al al])
      (cond [(null? al)
             (list (cons k (up '())))]
            [(=? (car (car al)) k)
             (cons
              (cons k (up (cdr (car al))))
              (cdr al))]
            [else
             (cons (car al) (loop (cdr al)))])))
  (define-values (base update)
    (cond [(equal? =? eq?)    (values (hasheq)  hash-update)]
          [(equal? =? eqv?)   (values (hasheqv) hash-update)]
          [(equal? =? equal?) (values (hash)    hash-update)]
          [else               (values '()       alist-update)]))
  (define classes
    (for/fold ([res base])
        ([elt (in-stream l)]
         [idx (in-naturals)])
      (define k (key elt))
      (define v (cons idx elt))
      (update res k (lambda (o) (cons v o)) '())))
  (define sorted-classes
    (if (list? classes)
        (for/list ([p (in-list classes)])
          (sort (cdr p) < #:key car))
        (for/list ([(_ c) (in-hash classes)])
          (sort c < #:key car))))
  (for/list ([c (in-list (sort sorted-classes < #:key caar))])
    (map cdr c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trace threading

(begin-for-syntax
  (define-syntax-class hole-expr
    #:datum-literals (_)
    (pattern (f:id x ... _ y ...)
             #:with proc #'(λ (z) (f x ... z y ...)))
    (pattern proc:expr))

  (define-splicing-syntax-class pipe
    (pattern (~seq #:take e:hole-expr)
             #:with proc #'(pipe-take e.proc))
    (pattern e:hole-expr
             #:with proc #'(pipe-test e.proc))
    (pattern #:repeat
             #:with proc #'(pipe-repeat)))
  )

;; Macro for "stream threading" that can provide a more declarative way
;; of specifying a stream predicate.
(define-syntax (stream~> stx)
  (syntax-parse stx
    [(_ ?t:expr ?p:pipe ...)
     #'(stream~>-pipeline (stream->list ?t)
                          (list ?p.proc ...))]))

;; The λ version of stream threading.
(define-syntax (λ-stream~> stx)
  (syntax-parse stx
    [(_ x ...)
     #'(λ (t) (stream~> t x ...))]))

;; Returns a stream predicate that computes whether a stream satisfies the
;; constraints given in the list of procedures.
(define (stream~>-pipeline t procs)
  (let/cc return
    (for/fold ([pre null]
               [post t])
              ([proc (in-list procs)])
      (cond
        [(pipe-take? proc)
         (when (empty? post)
           (return #t))
         (define pre* (proc post))
         (values pre* (drop post (length pre*)))]
        [(pipe-test? proc)
         (unless (proc pre)
           (return #f))
         (values pre post)]
        [(pipe-repeat? proc)
         (return (stream~>-pipeline post procs))]))
    #t))

(struct pipe-take (proc) #:property prop:procedure 0)
(struct pipe-test (proc) #:property prop:procedure 0)
(struct pipe-repeat ())
