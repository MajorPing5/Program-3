#lang racket

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
   returning the struct properly formatted for mapping|#
(define (reading_transaction_data data filename)
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
    [else (error (format "Incorrect transaction format: ~a" data))]))

;; The following is the dedicated function for reading in accounts.txt
(define (reading_accounts_data data filename)
  (match data
           [(list string_acct_num name string_balance)
            (let ([acct_num (string->number string_acct_num)]
                  [balance (string->number string_balance)])
            (user acct_num name balance))]
           [_ (error (format "Error reading data: ~a" data))]))

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
         (reading_accounts_data data filename)]

        ;; If the file being read is transactions
        [(string=? filename "TRANSACTIONS.TXT")
         (reading_transaction_data data filename)]

        ;; If the file being read is neither
        [else (error (format "File not found: ~a" filename))]))))

#|The following function will open and read in the data lines
  into a list (lines), before calling for File Manipulation|#
(define (process-file filename)
  (define lines (file->lines filename))
  (map (λ (line) (fileManip line filename)) lines))

(define (main)
  (define users (process-file "ACCOUNTS.TXT"))
  (for-each (λ (user)
              (printf "Account: ~a, Name: ~a, Balance: ~a\n"
                      (user-acct_num user)
                      (user-name user)
                      (user-balance user)))
              users)
  (define activity (process-file "TRANSACTIONS.TXT"))
  (for-each (λ (transaction)
              ((printf "Type: ~a, Account Number: ~a, Timestamp: ~a\n"
                      (transaction-type transaction)
                      (transaction-acct_num transaction)
                      (transaction-timestamp transaction))
              (cond
                [(string=? (transaction-type transaction) "Purchase")
                 (printf "Merchant: ~a, Amount: ~a"
                         (purchase-merchant purchase))]
                [(string=? (#|Currently working here. Find way to check if cash, check, or credit payment|#))])))
              activity))
  
(main)