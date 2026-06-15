#lang racket

;; common string constants for the render- functionalities 

(provide (all-defined-out))

(define STOCKs  "Stocks")
(define MUTUALs "Mutual Funds")
(define ETFs    "ETFs")
(define IRAs    "IRAs")
(define SEPs    "SEPs")
(define SUM     "; Sum ")

(define ACCOUNTs "Checking Accounts & CDs")

(define HOLDINGs "Holdings")

(define COST "Cost")
(define BASE "Base")
(define CURRENT "Value")
(define PROFIT "Profit")
(define HEADER (~a COST "     " BASE "      " CURRENT "     " PROFIT))

(define WHAT "Company/Fund/Account")

(define INDEX.html "index.html")
(define INDEX "The Index")
