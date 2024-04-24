#lang racket

#| The following functions are for triggering specific debugging calls, ranging from creating the output file
   to showing the math evaluations inbetween each evaluation. Should any of these be set to #t, then |#
(define create_output #f)

;; The following is intended to serve as the struct for user account information
(struct user
  (acct_num name balance) #:transparent)

(struct transaction (type acct_num timestamp merchant amount))

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

;; The following function below will decide which manipulation tactics to do based on the .txt it is being pulled from

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

  
;; The following function will open and read in the data lines into a list (lines), before calling for File Manipulation
(define (process-file filename)
  (let ([lines (file->lines filename)])
    (for ([line (in-list lines)])
      (fileManip line filename))))

;; This function is intended to execute if and only if line 5 is set to true
(define (create_output_check)
  (if create_output
      [call-with-output-file "STATEMENT.TXT"
        (λ (out-port) ;; If Line 5 is True, then this is where the output file is to be created and formatted
          [display "Output functions as intended." out-port])]
      (begin ;; If Line 5 is False
        (displayln "Output file has not been created due to initial Bool call.")
        (displayln "To create the output file, change line 5 to '#t' and try again"))))

#| The following is the main bulk of all debugging functions.
If all are off, then program will operate as normal.
If any are on, then the interaction panel will display the appropriate debugging call|#
(define (debugging_check)
  (create_output_check)
  ;; (output_accounts_check)
  #| (output_transactions_check)|#)
  
(define (main)
  (process-file "ACCOUNTS.TXT")
  (process-file "TRANSACTIONS.TXT")
  (debugging_check))

(main)