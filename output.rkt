#lang racket

(require "preprocessing.rkt")
(require "processing.rkt")

(provide (all-defined-out))

(define create-output #f)

;; Sorts the transactions based on their timestamp, using Timsort algo
(define (sort-transactions transactions)
  (sort transactions
        (lambda (a b)
          (< (transaction-timestamp a)
             (transaction-timestamp b)))))

;; Sorts the accounts based on their account number
(define (sort-accounts accounts)
  (sort accounts
        (lambda (a b)
          (< (user-acct-num a)
             (user-acct-num b)))))

;; Function to right-align a number to a specified width
(define (right-align num width)
  (let ([str (number->string num)])
    (string-append (make-string (- width (string-length str))
                                #\space) str)))

;; Function to truncate a string to a maximum length
(define (truncate str max-length)
  (if (> (string-length str) max-length)
      (substring str 0 max-length)
      str))

;; Function to format and create the output file
(define (create-output-file accounts transactions)
  (call-with-output-file "STATEMENT.TXT"
    (lambda (out-port)
      (for-each
       (lambda (account)
         (let* ([acct-num
                 (user-acct-num account)]
                [user-name
                 (user-name account)]
                [starting-balance
                 (user-balance account)]
                [filtered-transactions
                 (filter-by-account transactions acct-num)]
                [total-payments
                 (sum-payments filtered-transactions)]
                [total-purchases
                 (sum-purchases filtered-transactions)]
                [ending-balance
                 (calculate-ending-balance starting-balance
                                           total-payments
                                           total-purchases)]
                [sorted-transactions
                 (sort-transactions filtered-transactions)])
           ;; Write the statement header
           (fprintf out-port "STATEMENT OF ACCOUNT\n")
           (fprintf out-port "~a  ~a  Starting Balance: ~a\n\n"
                    acct-num user-name (right-align starting-balance 20))
           
           ;; Write the transactions
           (for-each
            (lambda (transaction)
              (let ([timestamp (transaction-timestamp transaction)]
                    [type (transaction-type transaction)]
                    [amount (cond
                              [(purchase? transaction)
                               (purchase-amount transaction)]
                              [(cash? transaction)
                               (cash-amount transaction)]
                              [(check? transaction)
                               (check-amount transaction)]
                              [(card? transaction)
                               (card-amount transaction)])]
                    [details (cond
                               [(purchase? transaction)
                                (truncate (purchase-merchant transaction)
                                          40)]
                               [(cash? transaction)
                                "Cash"]
                               [(check? transaction)
                                "Check"]
                               [(card? transaction)
                                (card-method transaction)])])
                (fprintf out-port "~a ~a ~a ~a\n"
                         timestamp
                         type
                         details
                         (right-align amount 20))))
            sorted-transactions)

           ;; Write the summary
           (fprintf out-port "\nTotal Purchases: ~a\n"
                    (right-align total-purchases 20))
           (fprintf out-port "Total Payments: ~a\n"
                    (right-align total-payments 20))
           (fprintf out-port "Ending Balance: ~a\n"
                    (right-align ending-balance 20))
           
           ;; Write the separator
           (fprintf out-port "\n******************************************
***************\n\n")))
       accounts))
    #:exists 'truncate))

;; This function is intended to execute only if line 5 is set to true
(define (create-output-check accounts transactions)
  (if create-output
      (let* ([sorted-accounts (sort-accounts accounts)]
             [sorted-transactions (sort-transactions transactions)])
        (create-output-file sorted-accounts sorted-transactions))
      (begin
        (displayln "Output file has not been created due to
 initial Bool call.")
        (displayln "To create the output file, change 'create-output'
 on line 8 to '#t' and try again"))))