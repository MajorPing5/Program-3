#lang racket

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
            (printf "User information - Account Number: ~a\n" acct_num)
            (if (number? acct_num)
                (printf "Account Number is in numeric data form\n")
                (printf "Account Number is still in string data form\n"))
            (printf " Name: ~a\n" name)
            (if (string? name)
                (printf "Account Number is in string data form\n")
                (printf "Account Number is in unknown data form\n"))
            (printf "Balance: ~a\n" balance)
            (if (number? balance)
                (printf "User Balance is in numeric data form\n")
                (printf "User Balance is still in string data form\n"))]
           [_ (error (format "Error reading file: ~a" filename))]))

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
  (process-file "ACCOUNTS.TXT"))
  
(main)