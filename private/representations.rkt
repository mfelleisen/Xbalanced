#lang racket

;; data representation of stocks, funds, etfs, and accounts

(provide
 ; sym+name
 accounts/c
 1security/c
 1security-records/c
 
 (contract-out
  [record-price     (-> 1record/c real?)]
  [record-cost      (-> 1record/c real?)]
  [record-shares#   (-> 1record/c real?)]
  [record-dividend? (-> 1record/c boolean?)]

  [all-syms+names
   (-> (listof 1security/c) (listof (list/c string? string?)))]

  [render-amounts-of-pennies
   (-> real? string?)]))

(module+ examples
  (provide (all-defined-out)))

;;---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require rackunit))

;                                                          
;                                                          
;                                               ;          
;                                               ;          
;   ;;;;    ;;;    ;;;    ;;;   ;   ;  ; ;;   ;;;;;   ;;;  
;       ;  ;;  ;  ;;  ;  ;; ;;  ;   ;  ;;  ;    ;    ;   ; 
;       ;  ;      ;      ;   ;  ;   ;  ;   ;    ;    ;     
;    ;;;;  ;      ;      ;   ;  ;   ;  ;   ;    ;     ;;;  
;   ;   ;  ;      ;      ;   ;  ;   ;  ;   ;    ;        ; 
;   ;   ;  ;;     ;;     ;; ;;  ;   ;  ;   ;    ;    ;   ; 
;    ;;;;   ;;;;   ;;;;   ;;;    ;;;;  ;   ;    ;;;   ;;;  
;                                                          
;                                                          
;                                                          

(define accounts/c
  (flat-murec-contract
   ([a (or/c empty? (flat-contract (cons/c string? b)))]
    [b (flat-contract (cons/c real? a))])
   a))

;                                                                        
;                                                                        
;                                         ;     ;       ;                
;                                               ;                        
;    ;;;    ;;;    ;;;   ;   ;   ;;;;   ;;;   ;;;;;   ;;;    ;;;    ;;;  
;   ;   ;  ;;  ;  ;;  ;  ;   ;   ;;  ;    ;     ;       ;   ;;  ;  ;   ; 
;   ;      ;   ;; ;      ;   ;   ;        ;     ;       ;   ;   ;; ;     
;    ;;;   ;;;;;; ;      ;   ;   ;        ;     ;       ;   ;;;;;;  ;;;  
;       ;  ;      ;      ;   ;   ;        ;     ;       ;   ;          ; 
;   ;   ;  ;      ;;     ;   ;   ;        ;     ;       ;   ;      ;   ; 
;    ;;;    ;;;;   ;;;;   ;;;;   ;      ;;;;;   ;;;   ;;;;;  ;;;;   ;;;  
;                                                                        
;                                                                        
;                                                                        

(define sym+name  #px"\\(([A-Z0-9]*)\\) (.*)")

(define [(regexp/c px) x]
  (regexp-match px x))

;; ---------------------------------------------------------------------------------------------------
;; due to random changes of digits the last per entry is _not_ equal to the product of the first two
;; otherwise I'd bake this into the contract
;; if it is interesting, we could create a data.ss file that lives up to those expectations 
(define 1record/c
  (or/c
    (list/c real? real? (or/c 'd) symbol? real?)
    (list/c real? real? symbol? real?)))

(define 1security-records/c (listof 1record/c))

(define 1security/c
  (cons/c (and/c string? (regexp/c sym+name)) 1security-records/c))

(define record-price first)

(define record-shares# second)

(define (record-dividend? l) (cons? (memq 'd (rest l))))

(define record-cost last)

(define (year-of l)
  (extract-year (symbol->string (list-ref l (if (record-dividend? l) 3 2)))))

#; {String -> Natural}
;; I am not sure what this really means :-) 
(define (extract-year astr)
  (string->number
   (substring astr (if (eq? (string-ref astr 5) #\/) 6 5) (string-length astr))))

(define (all-syms+names security*)
  (for/list ([stock security*])
    [match-define (cons line records) stock]
    (rest (regexp-match sym+name line))))

;                                                          
;                                                          
;                                      ;;;                 
;                                        ;                 
;    ;;;   ;   ;  ;;;;  ;;;;;;  ;;;;     ;     ;;;    ;;;  
;   ;;  ;   ; ;       ; ;  ;  ; ;; ;;    ;    ;;  ;  ;   ; 
;   ;   ;;  ;;;       ; ;  ;  ; ;   ;    ;    ;   ;; ;     
;   ;;;;;;   ;     ;;;; ;  ;  ; ;   ;    ;    ;;;;;;  ;;;  
;   ;       ;;;   ;   ; ;  ;  ; ;   ;    ;    ;          ; 
;   ;       ; ;   ;   ; ;  ;  ; ;; ;;    ;    ;      ;   ; 
;    ;;;;  ;   ;   ;;;; ;  ;  ; ;;;;      ;;   ;;;;   ;;;  
;                               ;                          
;                               ;                          
;                               ;                          

(module+ examples

  (define a1 '())
  (define a2 '("C1" 1.0 "C2" 2.0))

  (define y1 '1/1/1011)
  (define y2 '11/11/1011)

  (define r1 `(1 2 ,y1 3))
  (define rd `(1 2 d ,y1 3))

  (define n1 "(S) Stock1")
  (define nT "(T) StockT")

  (define s1 `(,n1 ,r1 ,rd))
  (define s2 `(,nT ,rd ,r1))
  (define s* (list s1 s2)))

;                                                   
;                                                   
;                                        ;          
;                                        ;          
;   ;;;;  ;;;;;;   ;;;   ;   ;  ; ;;   ;;;;;   ;;;  
;       ; ;  ;  ; ;; ;;  ;   ;  ;;  ;    ;    ;   ; 
;       ; ;  ;  ; ;   ;  ;   ;  ;   ;    ;    ;     
;    ;;;; ;  ;  ; ;   ;  ;   ;  ;   ;    ;     ;;;  
;   ;   ; ;  ;  ; ;   ;  ;   ;  ;   ;    ;        ; 
;   ;   ; ;  ;  ; ;; ;;  ;   ;  ;   ;    ;    ;   ; 
;    ;;;; ;  ;  ;  ;;;    ;;;;  ;   ;    ;;;   ;;;  
;                                                   
;                                                   
;                                                   

#;{Integer -> String}
(define (render-amounts-of-pennies i)
  (~r i #:precision '(= 2))) ; #:sign '+))

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test
  (check-equal? (render-amounts-of-pennies 1.) "1.00"))

(module+ test ;; records
  (check-equal? (extract-year (~a y1)) 11)
  (check-equal? (extract-year (~a y2)) 1011)
  
  (check-true (1record/c r1))
  (check-true (1record/c rd))
  (check-true (record-dividend? rd))

  (check-equal? (year-of r1) 11)
  (check-equal? (year-of rd) 11))

(module+ test ;; stocks
  (check-true (1security/c s1))
  (check-true (1security/c s2))

  (check-equal? (all-syms+names s*) (list (list "S" "Stock1") (list "T" "StockT"))))


(module+ test ;; accounts 
  (check-true (accounts/c a1))
  (check-true (accounts/c a2)))
