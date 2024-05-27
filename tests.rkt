#lang racket

;; Require your program module with renaming to avoid conflicts
(require rackunit
         rackunit/text-ui
         (rename-in "preprocessing.rkt" [check payment-check])
         "processing.rkt"
         "output.rkt")

;; Reset the transaction-counter for tests
(define (reset-transaction-counter) 10001)

(module+ test
  (define account-formatting
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
                    "Account Data is not being processed appropriately"))))
  
  (define transaction-formatting
    (test-suite
     "Transaction Formatting"
     ;; Tests the Purchase Formatting & Assignment
     (test-case
      "Purchase Transaction Formatting"
      (begin
        (reset-transaction-counter)
        (check-equal?
         (reading-transaction-data
          '("Purchase"
            "123456789012"
            "20200101"
            "Store"
            "100.00") 1)
         (purchase 10001
                   'purchase
                   123456789012
                   20200101
                   "Store"
                   100.00)
         "Purchase transaction data does not process correctly")))

     (test-case
      "Cash Payment Formatting"
      ;; Tests the Payment Payment Formatting & Assignment
      (begin
        (reset-transaction-counter)
        (check-equal?
         (reading-transaction-data
          '("Payment"
            "123456789012"
            "1234567"
            "Cash"
            "100.00") 1)
         (cash 10001
               'payment
               123456789012
               1234567
               'cash
               100.00)
         "Check Payment transaction data does not process correctly")))

     ;; Test Check payment transaction formatting & Assignment
     (test-case
      "Check Payment Formatting"
      (begin
        (reset-transaction-counter)
        (check-equal?
         (reading-transaction-data
          '("Payment"
            "123456789012"
            "20200101"
            "Check"
            "11101"
            "100.00") 1)
         (payment-check 10001
                        'payment
                        123456789012
                        20200101
                        'check
                        11101
                        100.00)
         "Check Payment transaction data does not process correctly")))

     ;; Test Credit payment transaction Formatting & Assignment
     (test-case
      "Credit Payment Formatting"
      (begin
        (reset-transaction-counter)
        (check-equal?
         (reading-transaction-data
          '("Payment"
            "123456789012"
            "20200101"
            "Credit"
            "550055005500"
            "100.00") 1)
         (card 10001
               'payment
               123456789012
               20200101
               'credit
               550055005500
               100.00)
         "Credit Payment transaction data does not process correctly")))
     
     (test-case
      "Debit Payment Formatting"
      (begin
        (reset-transaction-counter)
        (check-equal?
         (reading-transaction-data
          '("Payment"
            "123456789012"
            "20200101"
            "Debit"
            "550055005500"
            "100.00") 1)
         (card 10001
               'payment
               123456789012
               20200101
               'debit
               550055005500
               100.00)
         "Debit Payment transaction data does not process correctly")))))

  (define data-calculation
    (test-suite
     "Data Calculation"
     (test-case
      "Total Payment Calculator"
      ;; Test the calculation of payments with accuracy up to 1 cent
      (begin
        (reset-transaction-counter)
        (let* ([test-transactions (list
                                   (cash 10001
                                         'payment
                                         123456789012
                                         20200101
                                         'cash
                                         150.00)
                                   (payment-check 10002
                                                  'payment
                                                  123456789012
                                                  20200102
                                                  'check
                                                  11101
                                                  200.00)
                                   (card 10003
                                         'payment
                                         123456789012
                                         20200103
                                         'credit
                                         5500550055005500
                                         250.00))]
               ;; Filter transactions by account number 123456789012 
               [filtered-transactions (filter-by-account
                                       test-transactions 123456789012)])
          (check-= (sum-payments filtered-transactions) 600.00 0.01
                   "Payments are not calculated correctly"))))

     (test-case
      "Total Purchase Calculator"
      ;; Test the calculation of purchases with accuracy up to 1 cent
      (begin
        (reset-transaction-counter)
        (let ([transactions (list
                             (purchase 10001
                                       'purchase
                                       123456789012
                                       20200101
                                       "Store"
                                       100.00)
                             (purchase 10002
                                       'purchase
                                       123456789012
                                       20200102
                                       "Online"
                                       200.00))])
          (check-= (sum-purchases transactions) 300.00 0.01
                   "Purchases are not calculated correctly"))))
     (test-case
      "Balance Calculation"
      (begin
        (reset-transaction-counter)
        (let* ([total-purchases 900.00]
               [total-payments 500.00]
               [starting-balance 50.00])
          (check-= (calculate-ending-balance starting-balance
                                  total-payments
                                  total-purchases)
                   450.00 0.01
                   "Balance Calculation is not functioning as expected")
          )))))

  (define output-verification
    (test-suite
     "Output Formatting"
     (test-case
      "Output Testing"
      (begin
        (let*
            ([sample-accounts
              (list
               (user 123456789012 "John Doe" 1000.00)
               (user 234567890123 "Jane Smith" 1500.00))]
             [sample-transactions
              (list
               (purchase 10001
                         'purchase
                         123456789012
                         20200101
                         "Long Merchant Name That Exceeds Forty Characters"
                         200.00)
               (cash 10002
                     'payment
                     123456789012
                     20200102
                     'cash
                     300.00)
               (payment-check 10003
                      'payment
                      234567890123
                      20200103
                      'check
                      5001
                      400.00)
               (card 10004
                     'payment
                     234567890123
                     20200104
                     'credit
                     550055005500
                     500.00))]
             [out-port (open-output-string)]
             ;; Call the function with the string port
             [actual-output
              (with-output-to-string
                (lambda ()
                  (create-output-file sample-accounts
                                      sample-transactions)))]
             [expected-output
  "STATEMENT OF ACCOUNT
123456789012  John Doe    Starting Balance:              1000.00

20200101 Purchase Long Merchant Name That Exceeds Forty   200.00
20200102 Payment  Cash                                    300.00

Total Purchases:               200.00
Total Payments:                300.00
Ending Balance:               1100.00

*********************************************************
STATEMENT OF ACCOUNT
234567890123  Jane Smith  Starting Balance:              1500.00

20200103 Payment  Check                                   400.00
20200104 Payment  Credit                                  500.00

Total Purchases:                 0.00
Total Payments:                900.00
Ending Balance:               2400.00

*********************************************************
"])
          ;; Compare actual output with expected output
          (check-equal? actual-output expected-output
                        "Output does not match expected result"))))))
  
  ;; Run the tests
  (run-tests account-formatting)
  (run-tests transaction-formatting)
  (run-tests data-calculation)
  (run-tests output-verification))