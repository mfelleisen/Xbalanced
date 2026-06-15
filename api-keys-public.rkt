#lang racket

(provide (all-defined-out))

(require Xbalanced/private/common)

(define FUND-API (~a "http://127.0.0.1:" PORT "/~a/FUND=~a"))

(define ETF-API (~a "http://127.0.0.1:" PORT "/~a/ETF=~a"))

(define STOCK-KEY "this is normally an API key")

(define STOCK-API (~a "http://127.0.0.1:" PORT "/Stocks/STOCKS="))
