#lang racket

#| The following under this line will dictate if the ouput file is or is not to
be created. Enable/disable using either #t or #f appropriately |#
(define create_output #f)

;; The following 3 functions manage the input of the files for this assignment.

(define (process-file filename)
  (call-with-input-file filename
    (λ (in-port)
      (let loop ([line (read-line in-port)])
        (if (eof-object? line)
            (printf "\nEnd of file\n\n") ;; If the line is an end of line object
            (let* ([normalized-line (regexp-replace* #rx"\r|\n" (string-trim line) "")]
                   [data (string-split normalized-line #rx"(/s/s+)|[\t]+")]) ;; Look for double space or tab special character
              (displayln data)
              (loop (read-line in-port))))))))


;; This function is intended to execute if and only if line 5 is set to true
(define (create_output_check)
  (if create_output
      [call-with-output-file "STATEMENT.TXT"
        (λ (out-port) ;; If Line 5 is True
          [display "Output functions as intended." out-port])]
      (begin ;; If Line 5 is False
        (displayln "Output file has not been created due to initial Bool call.")
        (displayln "To create the output file, change line 5 to '#t' and try again"))))

(define (main)
  (process-file "ACCOUNTS.TXT")
  (process-file "TRANSACTIONS.TXT")
  (create_output_check))

(main)