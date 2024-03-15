#lang racket

#| The following under this line will dictate if an ouput is or is not to
be created. Enable/disable using either #t or #f appropriately |#
(define create_output #f)

;; The following 3 functions manage the input of the files for this assignment.

(define (process-account-line line)
  (regexp-split #px"  +" line)) ;; Separates by 2 spaces in accounts file

(define (process-transaction-line line)
  (string-split line)) ;; Separates by tab character in accounts file

(define (process-file filename process-line)
  (call-with-input-file filename
    (λ (in-port)
      (let loop ([line (read-line in-port)])
        (if (eof-object? line)
            (printf "\nEnd of file\n\n") ;; If the line is an end of line object
            (let ([data (process-line line)]) ;; If the line contains data
              (displayln data)
              (loop (read-line in-port))))))))


;; This function is intended to execute if and only if line 5 is set to true
(define (create_output_check)
  (if create_output
      [call-with-output-file "STATEMENT.TXT"
        (λ (out-port)
          [display "Output functions as intended." out-port])]
      (begin
        (displayln "Output file has not been created due to initial Bool call.")
        (displayln "To create the output file, change line 5 to '#t' and try again"))))

(define (main)
  (process-file "ACCOUNTS.TXT" process-account-line)
  (process-file "TRANSACTIONS.TXT" process-transaction-line)
  (create_output_check))

(main)