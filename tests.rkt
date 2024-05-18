#lang racket

(require rackunit)
(require rackunit/text-ui)

;; Require your program module with renaming to avoid conflicts
(require (rename-in "Program-3.rkt"
                    
#| Renames 'check' to 'program-check' or another suitable name |#
                    [check program-check]))  

(module+ test
  ;; Define all your unit tests here
  (define (test-account-parsing)
    ;; Test account data parser with appropriate formatting & placement
    (check-equal? (reading-accounts-data
                   '("123456789012" "John Doe" "500.00"))
                  (user 123456789012 "John Doe" 500.00)
                  "Account Data is not being processed appropriately"))

  (define (test-transaction-parsing)
    ;; Test parsing different types of transactions

    ;; Tests the Purchase transaction data parsing
    (check-equal? (reading-transaction-data
                   '("Purchase"
                     "123456789012"
                     "20200101"
                     "Store"
                     "100.00"))
                  (purchase "Purchase"
                            123456789012
                            20200101
                            "Store"
                            100.00)
                  "Purchase transaction data does not parse correctly")
    (check-equal?
     (reading-transaction-data '("Payment"
                                 "123456789012"
                                 "1234567"
                                 "Cash"
                                 "100.00"))
     (cash "Payment"
           123456789012
           1234567
           "Cash"
           100.00)
     "Check Payment transaction data does not parse correctly")
    ;; Test parsing check payment transaction
    (check-equal?
     (reading-transaction-data '("Payment"
                                 "123456789012"
                                 "20200101"
                                 "Check"
                                 "11101"
                                 "100.00"))
     (check "Payment"
            123456789012
            20200101
            "Check"
            11101
            100.00)
     "Check Payment transaction data does not parse correctly")

    ;; Test parsing credit payment transaction
    (check-equal?
     (reading-transaction-data
      '("Payment"
        "123456789012"
        "20200101"
        "Credit"
        "550055005500"
        "100.00"))
     (credit "Payment"
             123456789012
             20200101
             "Credit"
             550055005500
             100.00)
     "Credit Payment transaction data does not parse correctly"))

  (define (test-payment-calculation)
    ;; Test the calculation of payments
    (let* ([test-transactions (list
                               (cash "Payment"
                                     123456789012
                                     20200101
                                     "Cash"
                                     150.00)
                               (check "Payment"
                                      123456789012
                                      20200102
                                      "Check"
                                      11101
                                      200.00)
                               (credit "Payment"
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

  (define (test-purchase-calculation)
    ;; Test the calculation of purchases
    (let ([transactions (list (purchase "Purchase"
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
                    "Purchases are not calculated correctly")))

  ;; Run the tests
  (test-account-parsing)
  (test-transaction-parsing)
  (test-payment-calculation)
  (test-purchase-calculation)
)

(module+ main
  (run-tests 'test))