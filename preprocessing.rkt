#lang racket

(provide (all-defined-out))

#|The following is intended to serve as the struct for
  user account information|#
(struct user (acct-num name balance) #:transparent)

#| The following demonstrates a polymorphic struct data type for payments,
   allowing easier addition of different transaction types based on what's
   allowed and not without the need of adding more complex structs. |#
(struct transaction (id type acct-num timestamp) #:transparent)

(struct purchase transaction (merchant amount) #:transparent)
(struct cash transaction (method amount) #:transparent)
(struct check transaction (method chk-num amount) #:transparent)

;; Accounts for both debit and credit card payments
(struct card transaction (method card-num amount) #:transparent)

;; Creates a unique transaction upon intial read
(define (generate-transaction-id line-number)
  (+ (- line-number 1) 10001))
  
#| This function matches the data line currently read according
to its identifiers, separating the information appropriately before
returning the struct properly formatted for mapping.

Note: It is imperative that the transaction data lines are following the
formatting rules outlined in the assignment's documentation|#

(define (reading-transaction-data data line-number)
  (let ([id (generate-transaction-id line-number)])
  (match data
    [(list "Purchase" acct-num timestamp merchant amount)
     (purchase id
               'purchase
               (string->number acct-num)
               (string->number timestamp)
               merchant
               (string->number amount))]
    [(list "Payment" acct-num timestamp "Cash" amount)
     (cash id
           'payment
           (string->number acct-num)
           (string->number timestamp)
           'cash
           (string->number amount))]
    [(list "Payment" acct-num timestamp "Check" chk-num amount)
     (check id
            'payment
            (string->number acct-num)
            (string->number timestamp)
            'check
            (string->number chk-num)
            (string->number amount))]
    [(list "Payment" acct-num timestamp "Credit" card-num amount)
     (card id
             'payment
             (string->number acct-num)
             (string->number timestamp)
             'credit
             (string->number card-num)
             (string->number amount))]
    [(list "Payment" acct-num timestamp "Debit" card-num amount)
          (card id
             'payment
             (string->number acct-num)
             (string->number timestamp)
             'debit
             (string->number card-num)
             (string->number amount))]
    [else (error (format "Error reading transaction format: ~a\n"
                         data))])))

;; The following is the dedicated function for reading in accounts.txt
(define (reading-accounts-data data)
  (match data
    [(list acct-num name balance)
     (let ([acct-num (string->number acct-num)]
           [balance (string->number balance)])
       (user acct-num name balance))]
    [_ (error (format "Error reading file: ACCOUNTS.txt"))]))

;; This function is intended to clean up the lines read in
;; and send the information appropriate to the file it was read from
(define (fileManip line-number line filename)
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
         (reading-accounts-data data)]

        ;; If the file being read is transactions
        [(string=? filename "TRANSACTIONS.TXT")
         (reading-transaction-data data line-number)]

        ;; If the file being read is neither
        [else (error (format "File not found: ~a" filename))]))))

  
#|The following function will open and read in the data lines
  into a list (lines), before calling for File Manipulation|#
(define (process-file filename)
  (define lines (file->lines filename))
  (for/list ([line lines]
             [line-number (in-naturals)])
    (fileManip line-number line filename)))