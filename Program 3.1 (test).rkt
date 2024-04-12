#lang racket

#|The following holds the global reference for all properly processed data
  to ensure easy referencing|# 
(define processed-accounts '())

#|The following is intended to serve as the struct for
  user account information|#
(struct user
  (acct_num name balance) #:transparent)

;; The following is the dedicated function for reading in transaction.txt
(define (reading_transaction_data data filename)
  (displayln "Transaction data in process of being read"))

;; The following is the dedicated function for reading in accounts.txt
(define (reading_accounts_data data filename)
  (match data
           [(list string_acct_num name string_balance)
            (define acct_num (string->number string_acct_num))
            (define balance (string->number string_balance))
            (user acct_num name balance)
            (printf "User information - Account Number: ~a," acct_num)
            (printf " Name: ~a, Balance: ~a" name balance)]
           [_ (error (format "Error reading file: ~a" filename))]))

;; This function is intended to clean up the lines read in
;; and send the information appropriate to the file it was read from
(define (fileManip line filename)
  (begin
    (let* (;; Removes /r or /n special characters from lines
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
  (let ([lines (file->lines filename)])
    (map (λ (line)
           (let* (()))))))

(define (main)
  (process-file "ACCOUNTS.TXT"))
  
(main)