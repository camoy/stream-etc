#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [stream-group-by (->* (stream? (-> any/c any))
                        ((-> any/c any/c boolean?))
                        (listof list?))]
  [stream-map* (->i ([f (rst) (procedure-arity-includes/c (length rst))])
                    #:rest [rst (non-empty-listof stream?)]
                    [res stream?])]
  [stream-sum (-> (stream/c number?) number?)]
  [stream-member? (-> stream? any/c boolean?)])
 (rename-out [λ-stream~> lambda-stream~>])
 stream~>
 λ-stream~>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/function
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

;; Like `stream-map`, but variable-arity like `map`.
(define (stream-map* f . sts)
  (let go ([sts sts])
    (if (andmap (negate stream-empty?) sts)
        (stream-cons (apply f (map stream-first sts))
                     (go (map stream-rest sts)))
        empty-stream)))

;; Sums the numbers of a stream.
(define (stream-sum st)
  (stream-fold + 0 st))

;; Returns if a value is an element of a stream.
(define (stream-member? st x)
  (for/or ([y (in-stream st)])
    (equal? y x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; threading (data)

;; A `take-clause` is a clause that takes a prefix from the stream.
;;   - `proc` is a procedure that takes a stream and returns a prefix.
(struct take-clause (proc) #:property prop:procedure 0)

;; A `guard-clause` is a clause that checks the prefix against a predicate.
;;   - `proc` is a predicate over the current prefix.
(struct guard-clause (proc) #:property prop:procedure 0)

;; A `repeat-clause` should repeat the processing pipeline.
(struct repeat-clause ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; threading (syntax classes)

(begin-for-syntax
  ;; The RHS of a clause that potentially contains a hole.
  (define-syntax-class clause-expr
    #:datum-literals (_)
    (pattern (fn:expr pre:expr ... _ post:expr ...)
             #:with norm #'(λ (hole) (fn pre ... hole post ...)))
    (pattern norm:expr))

  ;; A `stream~>` clause.
  (define-splicing-syntax-class clause
    (pattern e:clause-expr
             #:with norm #'(guard-clause e.norm))
    (pattern (~seq #:take e:clause-expr)
             #:with norm #'(take-clause e.norm))
    (pattern #:repeat
             #:with norm #'(repeat-clause)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; threading operations

;; Macro for "stream threading" that can provide a more declarative way
;; of specifying a stream predicate.
(define-syntax (stream~> stx)
  (syntax-parse stx
    [(_ e:expr cl:clause ...+)
     #'(stream~>-run (stream->list e)
                     (list cl.norm ...))]))

;; The λ version of stream threading.
(define-syntax (λ-stream~> stx)
  (syntax-parse stx
    [(_ x ...)
     #'(λ (st) (stream~> st x ...))]))

;; Returns a stream predicate that computes whether a stream satisfies the
;; constraints given in the list of procedures.
(define (stream~>-run lst clauses)
  (let/cc return
    (for/fold ([pre null]
               [cur lst])
              ([clause (in-list clauses)])
      (cond
        [(take-clause? clause)
         (when (empty? cur)
           (return #t))
         (define pre* ((take-clause-proc clause) cur))
         (values pre* (drop cur (length pre*)))]
        [(guard-clause? clause)
         (unless ((guard-clause-proc clause) pre)
           (return #f))
         (values pre cur)]
        [(repeat-clause? clause)
         (return (stream~>-run cur clauses))]))
    #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           racket/function)

  (define (parity=? x y)
    (= (modulo x 2) (modulo y 2)))

  (define (ok? s)
    (stream~> s
              #:take (takef _ odd?)
              (negate empty?)
              #:take (takef _ zero?)
              #:repeat))

  (chk
   (stream-group-by '("1" "2" "3" "4") string->number parity=?)
   '(("1" "3") ("2" "4"))

   (stream->list (stream-map* add1 '(1 2 3)))  '(2 3 4)
   (stream->list (stream-map* + '(1 2 3) '(5 6 7)))  '(6 8 10)
   (stream->list (stream-map* + '(1 2 3) '(5)))  '(6)

   (stream-sum '()) 0
   (stream-sum '(1 2 3)) 6

   #:! #:t (stream-member? '() 42)
   #:! #:t (stream-member? '(1 2) 42)
   #:t (stream-member? '(1 42) 42)
   #:t (stream-member? '(42 1) 42)

   #:t (ok? '(1 3 0 0 1 7))
   #:t (ok? '(1 0 1 0 1 0))
   #:! #:t (ok? '(5 8))
   ))
