#lang racket

(provide (all-defined-out))
(require "preprocessing.rkt")

;; Creates a list of only the account numbers in the users account list
(define (only-account-numbers accounts-list)
  (map user-acct-num accounts-list))

;; Filters the list of accounts using a given account number
(define (searched-user-account all-accounts account-num)
  (first
   (filter
    (lambda (user)
      (= (user-acct-num user)
         account-num))
    all-accounts)))

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
                 [(card? transaction)
                  (+ (card-amount transaction) acc)]
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

#| Given a starting balance, total-payments, and total-purchases value,
calculate the amount to add or subtract from the starting balance based
on the change of money given through subtracting the total payments from
purchases. It also allows an easy point of diagnosis, considering that
the net change in any given day can be easily extrapolated if necessary|#
(define (calculate-ending-balance starting-balance
                                  total-payments
                                  total-purchases)
  (+ (- total-purchases total-payments) starting-balance))


(define (process-account all-transactions all-accounts account-num)
  (let* ([filtered-transactions (filter-by-account all-transactions
                                                   account-num)]
         [total-payments (sum-payments filtered-transactions)]
         [total-purchases (sum-purchases filtered-transactions)]
         [user-account (searched-user-account all-accounts account-num)]
         [ending-balance (calculate-ending-balance
                          (user-balance user-account)
                          total-payments
                          total-purchases)])
    ;; Output or return the aggregated data
    (values total-payments total-purchases ending-balance)))