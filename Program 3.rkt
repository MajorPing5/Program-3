#lang racket/gui

#| The following under this line will dictate if an ouput is or is not to
be created. Enable/disable using either #t or #f appropriately |#
(define create_output #f)

;; The following 3 functions manage the input/output of the files for this programming assignment.
(call-with-input-file "ACCOUNTS.TXT"
  (λ (in-port)
    (let loop()
      (let ([data (read-line in-port)])
        (unless (eof-object? data)
          (displayln data)
          (loop))))
    (printf "End of file\n\n")))

(call-with-input-file "TRANSACTIONS.TXT"
  (λ (in-port)
    (let loop()
      (let ([data (read-line in-port)])
        (unless (eof-object? data)
          (map (λ (line) (string-split line #\space)) lines)
          (displayln data)
          (loop))))
    (printf "\nEnd of file\n\n")))

(if create_output
  [call-with-output-file "STATEMENT.TXT"
    (λ (out-port)
      [display "Output functions as intended." out-port])]
  (begin
         (displayln "Output file has not been created due to initial Bool call.")
         (displayln "To create the output file, change line 5 to '#t' and try again")))

#|(define main
;; To Be Implemented
  )

(main)|#

#|
;; Read lines from a file (replace "your-file.txt" with the actual file path)
(define lines (read-line "your-file.txt"))

;; Split each line into words
(define words-list (map (λ (line) (string-split line #\space)) lines))

;; Print the words for each line
(for-each (λ (words) (displayln words)) words-list)
|#