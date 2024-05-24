#lang racket

(require "preprocessing.rkt")
(require "processing.rkt")
(require "output.rkt")
(provide main)

#| This is the start of the 'main' function, what is intended to
automatically execute when using the run function of the compiler|#
(define (main)
  (let ([accounts-list (process-file "ACCOUNTS.TXT")]
        [transactions-list (process-file "TRANSACTIONS.TXT")])
    ;; Generate the output file
    (create-output-check accounts-list transactions-list)))

(main)