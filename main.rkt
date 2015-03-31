#lang racket

;; Provides various RackUnit helpers for dealing with async channels

(provide check-unicast
         check-unicast-match

         ;; Checks that no message is sent on any of the given channels during the wait-time
         check-no-message)

;; ---------------------------------------------------------------------------------------------------

(require rackunit
         rackunit/log
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
       (quasisyntax/loc stx (check-unicast-internal channel
                                               expected-message
                                               description
                                               wait-time
                                               'loc
                                               (quote #,(syntax->datum stx)))))]))

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
    (with-check-info*
      (list (make-check-actual actual-message))
      (lambda ()
        (unless (equal? actual-message expected-message)
          (fail-check-with-message (format "Got ~s instead of ~a" actual-message description)))
        (test-log! #t)))))

(define-for-syntax (syntax->location stx)
  (list (syntax-source stx)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-position stx)
        (syntax-span stx)))

(define (fail-check-with-message message)
  (with-check-info (['message message])
    (fail-check)))

(define-syntax (check-unicast-match stx)
  (syntax-parse stx
    [(_ channel
        pattern
        (~or (~optional (~seq #:result result-exp) #:defaults ([result-exp #'(void)]))
             (~optional (~seq #:timeout wait-time) #:defaults ([wait-time #'default-wait-time]))) ...)
     (with-syntax ([loc (syntax->location stx)])
       (quasisyntax/loc stx
         (with-check-info (['name 'check-unicast-match]
                           ['location 'loc]
                           ['expression (quote #,(syntax->datum stx))])
           (define actual-message
             (or (sync/timeout wait-time channel) (fail-check-with-message "Timeout")))
           (with-check-info*
               (list (make-check-actual actual-message))
             (lambda ()
               (match actual-message
                 [pattern (test-log! #t) result-exp]
                 [_ (fail-check "The message did not match the expected pattern")]))))))]))

(define-syntax (check-no-message stx)
  (syntax-parse stx
    [(_ channels ... (~seq #:timeout wait-time))
     (with-syntax ([loc (syntax->location stx)])
       (quasisyntax/loc stx (check-no-message-internal (list channels ...)
                                                  wait-time
                                                  'loc
                                                  (quote #,(syntax->datum stx)))))]
    [(_ channels ...)
     (quasisyntax/loc stx (check-no-message channels ... #:timeout default-wait-time))]))

(define (check-no-message-internal channels wait-time loc expression)
  (with-check-info (['name 'check-no-message]
                    ['location loc]
                    ['expression expression])
    (define actual-message (apply sync/timeout wait-time channels))
    (when actual-message
      (fail-check-with-message (format "One of the channels received message ~a" actual-message)))
    (test-log! #t)))
