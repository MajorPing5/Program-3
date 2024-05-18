#lang racket

(provide (all-defined-out))
(require "preprocessing.rkt")

;; Creates a list of only the account numbers in the users account list
(define (only-account-numbers accounts-list)
  (map user-acct-num accounts-list))

;; Filters the list of transactions using a given account number
(define (filter-by-account all-transactions account-number)
  (filter (λ (transaction)
            (= (transaction-acct-num transaction) account-number))
          all-transactions))

#| Given a list of filtered transactions under 1 user account number,
use the amount tag for each payment and add them together to create the
sum payment amount for the account's history|#
(define (sum-payments filtered-transactions)
  (foldl (lambda (transaction acc)
           (cond [(cash? transaction)
                  (+ (cash-amount transaction) acc)]
                 [(check? transaction)
                  (+ (check-amount transaction) acc)]
                 [(credit? transaction)
                  (+ (credit-amount transaction) acc)]
                 [else acc]))
         0
         filtered-transactions))

#| Given a list of filtered transactions under 1 user account number,
use the amount tag for each purchase and add them together to create the
sum purchase amount for the account's history|#
(define (sum-purchases filtered-transactions)
  (foldl (lambda (transaction acc)
           (if (purchase? transaction)
               (+ (purchase-amount transaction) acc)
               acc))
         0
         filtered-transactions))

(define (process-account all-transactions account-num)
  (let* ([filtered-transactions
          (filter-by-account all-transactions account-num)]
         [total-payments (sum-payments filtered-transactions)]
         [total-purchases (sum-purchases filtered-transactions)])
    ;; Output or return the aggregated data
    (values total-payments total-purchases)))
