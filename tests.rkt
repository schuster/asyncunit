#lang racket

(require "main.rkt"
         rackunit
         racket/async-channel)

(test-case "Good match for check-unicast-match"
  (define c (make-async-channel))
  (async-channel-put c (list 1 2 3))
  (define a (check-unicast-match c (list 1 2 x) #:result x))
  (check-equal? a 3))

(test-case "Bad match for check-unicast-match"
  (define c (make-async-channel))
  (async-channel-put c (list 1 2 3))
  (check-exn (lambda (x) #t) (lambda () (check-unicast-match c (list 1 3 x) #:result x))))

(test-case "Timeout for check-unicast-match"
  (check-exn (lambda (x) #t) (lambda () (check-unicast-match (make-async-channel) (list 1 2 x)))))
