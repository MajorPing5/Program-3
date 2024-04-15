#lang racket

#| The following functions are for triggering specific debugging calls,
ranging from creating the output file to showing the math evaluations
inbetween each evaluation. Should any of these be set to #t, then the
debugging function WILL execute|#
(define create_output #f)
(define display-user-struct-information #f)
(define display-transaction-struct-information #f)

#|The following is intended to serve as the struct for
  user account information|#
(struct user (acct_num name balance) #:transparent)

#| The following demonstrates a polymorphic struct data type for payments,
   allowing easier addition of different payment types based on what's
   allowed and not without the need of adding more complex structs. |#
(struct transaction (type acct_num timestamp) #:transparent)

(struct purchase transaction (merchant amount) #:transparent)
(struct cash transaction (method amount) #:transparent)
(struct check transaction (method chk_num amount) #:transparent)

 ;; Accounts for both debit and credit card payments
(struct credit transaction (method card_num amount) #:transparent)

#| This function matches the data line currently read according
to its identifiers, separating the information appropriately before
returning the struct properly formatted for mapping.

Note: It is imperative that the transaction data lines are following the
formatting rules outlined in the assignment's documentation|#

(define (reading_transaction_data data)
  (match data
    [(list "Purchase" acct_num timestamp merchant amount)
     (purchase "Purchase"
               (string->number acct_num)
               (string->number timestamp)
               merchant
               (string->number amount))]
    [(list "Payment" acct_num timestamp "Cash" amount)
     (cash "Payment"
           (string->number acct_num)
           (string->number timestamp)
           "Cash"
           (string->number amount))]
    [(list "Payment" acct_num timestamp "Check" chk_num amount)
     (check "Payment"
            (string->number acct_num)
            (string->number timestamp)
            "Check"
            (string->number chk_num)
            (string->number amount))]
    [(list "Payment" acct_num timestamp "Credit" card_num amount)
     (credit "Payment"
             (string->number acct_num)
             (string->number timestamp)
             "Credit"
             (string->number card_num)
             (string->number amount))]
    [else (error (format "Error reading transaction format: ~a\n"
                         data))]))

;; The following is the dedicated function for reading in accounts.txt
(define (reading_accounts_data data)
  (match data
           [(list string_acct_num name string_balance)
            (let ([acct_num (string->number string_acct_num)]
                  [balance (string->number string_balance)])
            (user acct_num name balance))]
           [_ (error (format "Error reading file: ACCOUNTS.txt"))]))

;; This function is intended to clean up the lines read in
;; and send the information appropriate to the file it was read from
(define (fileManip line filename)
  (begin
    (let* (;; Removes /r, /n, and \" special characters from lines
           [cleaned-line (regexp-replace* #rx"\r|\n|[\"]" line "")]

           ;; Trims lines for excess spaces after removal of spec chars
           [trimmed-line (string-trim cleaned-line)]
         
           ;; Look for double space or tab special character exclusively
           [data (regexp-split #px"[ ]{2,}|[\t]+" trimmed-line)])
   
      (cond
        ;; If the file being read is accounts
        [(string=? filename "ACCOUNTS.TXT")
         (reading_accounts_data data)]

        ;; If the file being read is transactions
        [(string=? filename "TRANSACTIONS.TXT")
         (reading_transaction_data data)]

        ;; If the file being read is neither
        [else (error (format "File not found: ~a" filename))]))))

  
#|The following function will open and read in the data lines
  into a list (lines), before calling for File Manipulation|#
(define (process-file filename)
  (define lines (file->lines filename))
  (map (λ (line) (fileManip line filename)) lines))

;; This function is intended to execute only if line 5 is set to true
(define (create_output_check)
  (if create_output
      [call-with-output-file "STATEMENT.TXT"
        (λ (out-port) ;; If Line 5 is True...
          [display "Output functions as intended." out-port])]
      (begin ;; If Line 5 is False
        (displayln "Output file has not been created due to initial Bool
call.")
        (displayln "To create the output file, change line 7 to '#t' and
try again"))))

#| For debugging purposes, the following functions are purely intended to
serve as information functions to help track the state of all identifiers
throughout the program|#

(define (show-user-struct-info accounts-list)
  (for-each (λ (user)
              (printf "Account: ~a, Name: ~a, Balance: ~a\n"
                      (user-acct_num user)
                      (user-name user)
                      (user-balance user)))
            accounts-list))

(define (show-transaction-struct-info transactions-list)
  (for-each (λ (transaction)
              (printf "Type: ~a, Account Number: ~a, Timestamp: ~a, "
                      (transaction-type transaction)
                      (transaction-acct_num transaction)
                      (transaction-timestamp transaction))
              (cond
                [(purchase? transaction)
                 (printf "Merchant: ~a, Amount: ~a\n"
                         (purchase-merchant transaction)
                         (purchase-amount transaction))]
                [(cash? transaction)
                 (printf "Method: ~a, Amount: ~a\n"
                         (cash-method transaction)
                         (cash-amount transaction))]
                [(check? transaction)
                 (printf "Method: ~a, Check Number: ~a, Amount: ~a\n"
                         (check-method transaction)
                         (check-chk_num transaction)
                         (check-amount transaction))]
                [(credit? transaction)
                 (printf "Method: ~a, Card Number: ~a, Amount: ~a\n"
                         (credit-method transaction)
                         (credit-card_num transaction)
                         (credit-amount transaction))]))
              transactions-list))

#| The following is the main bulk of all debugging functions.
If all are off, then program will ignore everything and run as normal.
If any are on, then the interaction panel will display the appropriate
debugging call. Check lines 7-x for which debugging check you want|#

(define (debugging_check accounts-list transactions-list)
  (begin
    (if display-user-struct-information
        (show-user-struct-info accounts-list)
        (void))
    (if display-transaction-struct-information
        (show-transaction-struct-info transactions-list)
        (void))
  (create_output_check)))

(define (only-account-numbers accounts-list)
  (map user-acct_num accounts-list))

(define (filter-by-account structs account-number)
  (filter (λ (struct)
            (= (transaction-acct_num struct) account-number))
          structs))

(define (sum-payments transactions)
  (foldl (lambda (transaction acc)
           (if (or (cash? transaction) (check? transaction) (credit? transaction))
               (+ (transaction-amount transaction) acc)
               acc))
         0
         transactions))

(define (sum-purchases transactions)
  (foldl (lambda (transaction acc)
           (if (purchase? transaction)
               (+ (purchase-amount transaction) acc)
               acc))
         0
         transactions))

(define (process-account transactions-list account-num)
  (let ((filtered-transactions (filter-transactions-by-account transactions-list account-num)))
    (let ((total-payments (sum-payments filtered-transactions))
          (total-purchases (sum-purchases filtered-transactions)))
      ;; Output or return the aggregated data
      (values total-payments total-purchases))))

#| This is the start of the 'main' function, what is intended to
automatically execute when using the run function of the compiler|#
(define (main)
  (define accounts-list (process-file "ACCOUNTS.TXT"))
  (define transactions-list (process-file "TRANSACTIONS.TXT"))

    (for-each (lambda (account-num)
              (let-values (((total-payments total-purchases)
                            (process-account transactions-list account-num)))
                (printf "Account: ~a\nTotal Payments: ~a\nTotal Purchases: ~a\n"
                        account-num total-payments total-purchases)))
            account-numbers)
  
  (debugging_check accounts-list transactions-list))

(main)