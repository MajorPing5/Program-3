#lang racket

#| The following functions are for triggering specific debugging calls, ranging from creating the output file
   to showing the math evaluations inbetween each evaluation. Should any of these be set to #t, then |#
(define create_output #f)

;; The following is intended to serve as the struct for user account information
(struct user
  (acct_num name balance) #:transparent)

;; The following is the dedicated function for reading in transaction.txt
(define (reading_transaction_data data filename)
  (displayln "Transaction data in process of being read"))

;; The following is the dedicated function for reading in accounts.txt
(define (reading_accounts_data data filename)
  (map (λ (data)
         (match data
           [(list acct_num name balance)
            (user (string->number acct_num) name (string->number balance))]
           [_ (error (format "Error reading file: ~a" filename))])))

;; The following 3 functions manage the input of the files for this assignment.
(define (process-file filename)
  (call-with-input-file filename
    (λ (in-port)
      (let loop ([line (read-line in-port)])
        (if (eof-object? line)
            (printf "\nEnd of file\n\n") ;; If the line is an end of line object
            (let* ([normalized-line (regexp-replace* #rx"\r|\n" line "")] ;; Removes /r or /n special characters from lines
                   [refined-line (string-trim normalized-line)]           ;; Trims lines for excess spaces after removal of spec chars
                   [data (regexp-split #px"[ ]{2,}|[\t]+" refined-line)]) ;; Look for double space or tab special character exclusively
              (cond [(string=? filename "ACCOUNTS.TXT") ;; If the file being read is accounts
                     (reading_accounts_data data filename)]
                    [(string=? filename "TRANSACTIONS.TXT")
                     (reading_transaction_data data filename)]
                    [else (error (format "File not recognized: ~a" filename))])
              (displayln data)
              (loop (read-line in-port))))))))

;; This function is intended to execute if and only if line 5 is set to true
(define (create_output_check)
  (if create_output
      [call-with-output-file "STATEMENT.TXT"
        (λ (out-port) ;; If Line 5 is True, then this is where the output file is to be created and formatted
          [display "Output functions as intended." out-port])]
      (begin ;; If Line 5 is False
        (displayln "Output file has not been created due to initial Bool call.")
        (displayln "To create the output file, change line 5 to '#t' and try again"))))

#| This function is intended to execute if I need to ensure proper data input is being passed.
   This is purely for debugging purposes only. |#
(define (output_accounts_check)
  (if show_accounts
      [for-each ]) ;; Currently working HERE!

#| The following is the main bulk of all debugging functions.
If all are off, then program will operate as normal.
If any are on, then the interaction panel will display the appropriate debugging call|#
(define (debugging_check)
  (create_output_check)
  (output_accounts_check)
  (output_transactions_check))
  
(define (main)
  (process-file "ACCOUNTS.TXT")
  (process-file "TRANSACTIONS.TXT")
  (debugging_check))

(main)