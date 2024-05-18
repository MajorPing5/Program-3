#lang racket

(require "preprocessing.rkt")
(require "processing.rkt")
(require "output.rkt")
(provide main)

#| This is the start of the 'main' function, what is intended to
automatically execute when using the run function of the compiler|#
(define (main)
  (define accounts-list (process-file "ACCOUNTS.TXT"))
  (define transactions-list (process-file "TRANSACTIONS.TXT"))

  (for-each (lambda (account-num)
              (let-values (((total-payments total-purchases)
                            (process-account
                             transactions-list account-num)))
                (printf
                 "Account: ~a\nTotal Payments: ~a\nTotal Purchases: ~a\n"
                 account-num
                 total-payments
                 total-purchases)))
            (only-account-numbers accounts-list)))

(main)