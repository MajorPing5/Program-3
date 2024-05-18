#lang racket

(require "processing.rkt")

(provide (all-defined-out))

(define create-output #f)

;; This function is intended to execute only if line 5 is set to true
(define (create-output-check)
  (if create-output
      [call-with-output-file "STATEMENT.TXT"
        (λ (out-port) ;; If Line 5 is True...
          [display "Output functions as intended." out-port])]
      (begin ;; If Line 5 is False
        (displayln "Output file has not been created due to initial Bool
call.")
        (displayln "To create the output file, change line 7 in
'output.rkt' to '#t' and try again"))))