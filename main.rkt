#lang racket

;; Provides various RackUnit helpers for dealing with async channels

(provide check-unicast)

;; ---------------------------------------------------------------------------------------------------

(require rackunit
         (for-syntax syntax/parse))

(define default-wait-time 2)

(define-syntax (check-unicast stx)
  (syntax-parse stx
    [(_ channel
        expected-message
        (~or (~optional (~seq #:description description)
                        #:defaults ([description #'"expected message"]))
             (~optional (~seq #:timeout wait-time) #:defaults ([wait-time #'default-wait-time]))) ...)
     (with-syntax ([loc (syntax->location stx)])
       #'(check-unicast-internal channel
                                 expected-message
                                 description
                                 wait-time
                                 'loc
                                 (quote #,(syntax->datum stx))))]))

(define (check-unicast-internal expected-channel
                                expected-message
                                description
                                wait-time
                                loc
                                expression)
  (with-check-info (['name 'check-unicast]
                    ['location loc]
                    ['expression expression])
    (define actual-message
      (or (sync/timeout wait-time expected-channel)
          (fail-check-with-message (format "Timeout while waiting on ~a" description))))
    (unless (equal? actual-message expected-message)
      (fail-check-with-message (format "Got ~s instead of ~a" actual-message description)))))

(define-for-syntax (syntax->location stx)
  (list (syntax-source stx)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-position stx)
        (syntax-span stx)))

(define (fail-check-with-message message)
  (with-check-info (['message message])
    (fail-check)))
