#lang racket

;; create index of all HTML files

(provide
 WHERE ;; placed here because I couldn't think of a better place; it's really shared among render(
 
 
 (contract-out
  [print-index-lines (-> index? any)]
  [make-index        (-> index?)]))

(module+ examples
  (provide (all-defined-out)))

;; ---------------------------------------------------------------------------------------------------
(require Xbalanced/private/dates)

(module+ examples
  (require (submod Xbalanced/private/dates examples)))

(module+ test
  (require (submod ".." examples))
  (require (submod Xbalanced/private/dates examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define WHERE "Checks/")

(struct index (lines))

(module+ examples
  (define index1-rendered (string-join (map (λ (x) (format template x x)) d*-sorted) ""))
  (define index1 (index d*-sorted)))

;; ---------------------------------------------------------------------------------------------------
(define template "<a href=~s>~a</a><br />\n")

(define [(print-index-lines an-index)]
  (for ([x (index-lines an-index)])
    (printf template x x)))

(module+ test
  (check-equal? (with-output-to-string [print-index-lines index1]) index1-rendered))

;; ---------------------------------------------------------------------------------------------------
(define (make-index)
  (parameterize ([current-directory WHERE])
    (index (create-index-from (directory-list)))))

#; {[Listof Path] -> [Listof String]}
;; sorted paths from `dl` that are dated html summaries of assessments 
(define (create-index-from dl)
  (define file* (map path->string dl))
  (define html* (filter (λ (x) (regexp-match #px"\\.html$" x)) file*))
  (define all   (filter-map (λ (x) (and (filename->date x) x)) html*)) 
  (sort all (compose not <-short-date) #:key filename->date))

;; TODO: filter html files first

(module+ test
  (define d*.html (map (λ (x) (~a x ".html")) d*))
  (define d*-sorted.html (map (λ (x) (~a x ".html")) d*-sorted))
  (check-equal? (create-index-from (map string->path d*.html)) d*-sorted.html))

