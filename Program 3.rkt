#lang racket

#| The following functions are for triggering specific debugging calls, ranging from creating the output file
   to showing the math evaluations inbetween each evaluation. Should any of these be set to #t, then |#
(define create_output #f)

;; The following is intended to serve as the struct for user account information
(struct user
  (acct_num name balance) #:transparent)

;; The following holds the global reference for all properly processed data to ensure easy referencing
(define processed-accounts '())

;; The following is the dedicated function for reading in transaction.txt
(define (reading_transaction_data data filename)
  (displayln "Transaction data in process of being read"))

;; The following is the dedicated function for reading in accounts.txt
(define (reading_accounts_data data filename)
  (λ (data filename)
         (match data
           [(list acct_num name balance)
            (user (string->number acct_num) name (string->number balance))]
           [_ (error (format "Error reading file: ~a" filename))])))

;; The following function below will decide which manipulation tactics to do based on the .txt it is being pulled from

;; The following function below will now manipulate the information based on the .txt being pulled
(define (fileManip line filename)
(begin
  (let* ([cleaned-line (regexp-replace* #rx"\r|\n" line "")] ;; Removes /r or /n special characters from lines
         [trimmed-line (string-trim cleaned-line)]           ;; Trims lines for excess spaces after removal of spec chars
         [data (regexp-split #px"[ ]{2,}|[\t]+" trimmed-line)]) ;; Look for double space or tab special character exclusively
    (cond [(string=? filename "ACCOUNTS.TXT") ;; If the file being read is accounts
           (reading_accounts_data data filename)
           (cons user processed-accounts)]
          [(string=? filename "TRANSACTIONS.TXT")
           (reading_transaction_data data filename)]
          [else (error (format "File not found: ~a" filename))])
    (displayln data))))
  
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