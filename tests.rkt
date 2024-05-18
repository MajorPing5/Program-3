#lang racket

;; Require your program module with renaming to avoid conflicts
(require (rename-in rackunit
                    #| Renames 'check' to 'rackunit-check' for rackuinit module,
only local to this file |#
                    [check rackunit-check]))
(require (rename-in "preprocessing.rkt"
                    #| Renames 'check' to 'program-check' from preprocessing.rkt file,
only locally to this file |#
                    [check payment-check]))


(require "processing.rkt")

;; Reset the transaction-counter for tests
(define (reset-transaction-counter)
  (transaction-counter (λ (_) 10001)))

(module+ test
  (define pre-processing-tests
    ;; Define all your unit tests here
    (test-suite
     "Data Formatting"
     #:before (λ () (reset-transaction-counter))
     (test-case
      "Account Formatting"
      ;; Tests Account data formatting & assignment
      (check-equal? (reading-accounts-data
                     '("123456789012" "John Doe" "500.00"))
                    (user 123456789012 "John Doe" 500.00)
                    "Account Data is not being processed appropriately"))

    (test-suite
     "Transaction Formatting"
     #:before (λ () (reset-transaction-counter))
     ;; Tests the Purchase transaction data parsing
     (test-case
      "Purchase Transaction Formatting"
      (check-equal?
       (reading-transaction-data
        '("Purchase"
          "123456789012"
          "20200101"
          "Store"
          "100.00"))
       (purchase 10001
                 "Purchase"
                 123456789012
                 20200101
                 "Store"
                 100.00)
       "Purchase transaction data does not parse correctly"))
     (test-case
      "Payment Transaction Formatting"
      ;; Tests the Payment Payment Formatting & Assignment
      (check-equal?
       (reading-transaction-data
        '("Payment"
          "123456789012"
          "1234567"
          "Cash"
          "100.00"))
       (cash 10001
             "Payment"
             123456789012
             1234567
             "Cash"
             100.00)
       "Check Payment transaction data does not parse correctly"))
     ;; Test parsing check payment transaction
     (test-case
      "Check Payment Formatting"
      (check-equal?
       (reading-transaction-data
        '("Payment"
          "123456789012"
          "20200101"
          "Check"
          "11101"
          "100.00"))
       (payment-check 10001
                      "Payment"
                      123456789012
                      20200101
                      "Check"
                      11101
                      100.00)
       "Check Payment transaction data does not parse correctly"))

     ;; Test parsing credit payment transaction
     (test-case
      "Credit Payment Formatting"
      (check-equal?
       (reading-transaction-data
        '("Payment"
          "123456789012"
          "20200101"
          "Credit"
          "550055005500"
          "100.00"))
       (credit 10001
               "Payment"
               123456789012
               20200101
               "Credit"
               550055005500
               100.00)
       "Credit Payment transaction data does not parse correctly"))))
    (test-suite
     "Data Calculation"
     (test-case
      "Total Payment Calculator"
       ;; Test the calculation of payments
       (let* ([test-transactions (list
                                  (cash 10001
                                        "Payment"
                                        123456789012
                                        20200101
                                        "Cash"
                                        150.00)
                                  (payment-check 10002
                                                 "Payment"
                                                 123456789012
                                                 20200102
                                                 "Check"
                                                 11101
                                                 200.00)
                                  (credit 10003
                                          "Payment"
                                          123456789012
                                          20200103
                                          "Credit"
                                          5500550055005500
                                          250.00))]
              ;; Filter transactions by account number 123456789012
              [filtered-transactions (filter-by-account
                                      test-transactions 123456789012)])
         (check-equal? (sum-payments filtered-transactions) 600.00
                       "Payments are not calculated correctly")))

     (test-case
      "Total Purchase Calculator"
       ;; Test the calculation of purchases
       (let ([transactions (list
                            (purchase "Purchase"
                                      123456789012
                                      20200101
                                      "Store"
                                      100.00)
                            (purchase "Purchase"
                                      123456789012
                                      20200102
                                      "Online"
                                      200.00))])
         (check-equal? (sum-purchases transactions) 300.00
                       "Purchases are not calculated correctly")))))

     ;; Run the tests
     (test-account-parsing)
     (test-transaction-parsing)
     (test-payment-calculation)
     (test-purchase-calculation)
     )