#lang racket/gui

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
          (displayln data)
          (loop))))
    (printf "\nEnd of file\n\n")))
