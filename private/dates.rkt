#lang racket

;; connect filenames with dates

(define short-date? (list/c natural? natural? natural?))

(provide
 (contract-out
  [date->filename (-> string? string?)]
  [filename->date (-> string? (or/c short-date? #false))]
  [<-short-date   (-> short-date? short-date? boolean?)]))

(module+ examples
  (provide (all-defined-out)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".." examples))
  (require rackunit))

;                                                                 
;       ;                                                         
;       ;           ;                                             
;       ;           ;                                             
;    ;;;;  ;;;;   ;;;;;  ;;;;           ;;;;   ;;;   ;;;;         
;   ;; ;;      ;    ;        ;          ;;  ; ;;  ;  ;; ;;        
;   ;   ;      ;    ;        ;          ;     ;   ;; ;   ;        
;   ;   ;   ;;;;    ;     ;;;;          ;     ;;;;;; ;   ;        
;   ;   ;  ;   ;    ;    ;   ;          ;     ;      ;   ;        
;   ;; ;;  ;   ;    ;    ;   ;          ;     ;      ;; ;;   ;;   
;    ;;;;   ;;;;    ;;;   ;;;;          ;      ;;;;  ;;;;    ;;   
;                                                    ;            
;                                                    ;            
;                                                    ;            

#; {type Date = [list 2Nat 2Nat 2Nat]}

(define short-date-year first)
(define short-date-month second)
(define short-date-day third)

(define dat "([0-9][0-9]).([0-9][0-9]).([0-9][0-9])")

(module+ examples
  (define p1 "26.02")
  
  (define d1 "26.02.01")
  (define d2 "25.12.12")
  (define d3 "25.12.13")
  (define d4 "25.12.11")
  (define d5 "99.12.11")
  (define d6 "99.12.22")

  (define d* (list d6 d1 d2 d3 d4 d5))
  (define d*-sorted (list d1 d3 d2 d4 d6 d5)))

;                                                                               
;                           ;;                                                  
;     ;                    ;       ;   ;;;                                      
;     ;                    ;             ;                                      
;   ;;;;;   ;;;          ;;;;;   ;;;     ;     ;;;   ; ;;   ;;;;  ;;;;;;   ;;;  
;     ;    ;; ;;           ;       ;     ;    ;;  ;  ;;  ;      ; ;  ;  ; ;;  ; 
;     ;    ;   ;           ;       ;     ;    ;   ;; ;   ;      ; ;  ;  ; ;   ;;
;     ;    ;   ;           ;       ;     ;    ;;;;;; ;   ;   ;;;; ;  ;  ; ;;;;;;
;     ;    ;   ;           ;       ;     ;    ;      ;   ;  ;   ; ;  ;  ; ;     
;     ;    ;; ;;           ;       ;     ;    ;      ;   ;  ;   ; ;  ;  ; ;     
;     ;;;   ;;;            ;     ;;;;;    ;;   ;;;;  ;   ;   ;;;; ;  ;  ;  ;;;; 
;                                                                               
;                                                                               
;                                                                               

#; {-> String}
(define (date->filename where)
  (date->filename/internal where (current-seconds)))

#; {Natural -> Date}
(define (date->filename/internal where s)
  (define d (seconds->date s))
  (define y (exactly-2-digits (- (date-year d) 2000)))
  (define m (exactly-2-digits (date-month d)))
  (define a (exactly-2-digits (date-day d)))
  (define file-name (string-append y "." m "." a))
  (~a where file-name))

(define (exactly-2-digits n)
  (~a n #:min-width 2 #:max-width 2 #:align 'right #:left-pad-string "0"))

(module+ test
  (check-equal? (date->filename/internal "" 1781283608) "26.06.12"))

;                                                                                             
;      ;;                                 ;;                                                  
;     ;                                  ;       ;   ;;;                                      
;     ;                                  ;             ;                                      
;   ;;;;;   ;;;;   ;;;  ;;;;;;         ;;;;;   ;;;     ;     ;;;   ; ;;   ;;;;  ;;;;;;   ;;;  
;     ;     ;;  ; ;; ;; ;  ;  ;          ;       ;     ;    ;;  ;  ;;  ;      ; ;  ;  ; ;;  ; 
;     ;     ;     ;   ; ;  ;  ;          ;       ;     ;    ;   ;; ;   ;      ; ;  ;  ; ;   ;;
;     ;     ;     ;   ; ;  ;  ;          ;       ;     ;    ;;;;;; ;   ;   ;;;; ;  ;  ; ;;;;;;
;     ;     ;     ;   ; ;  ;  ;          ;       ;     ;    ;      ;   ;  ;   ; ;  ;  ; ;     
;     ;     ;     ;; ;; ;  ;  ;          ;       ;     ;    ;      ;   ;  ;   ; ;  ;  ; ;     
;     ;     ;      ;;;  ;  ;  ;          ;     ;;;;;    ;;   ;;;;  ;   ;   ;;;; ;  ;  ;  ;;;; 
;                                                                                             
;                                                                                             
;                                                                                             

#; {String -> Date}
(define (filename->date x)
  (define check (regexp-match dat x))
  (and check (map string->number (rest check))))

(module+ test
  (check-false (filename->date p1))
  (check-equal? (filename->date d1) '(26 2 1))
  (check-equal? (filename->date d2) '(25 12 12)))

;                                                                               
;          ;                                      ;                             
;          ;                      ;               ;           ;                 
;          ;                      ;               ;           ;                 
;    ;;;   ; ;;    ;;;    ;;;;  ;;;;;          ;;;;  ;;;;   ;;;;;   ;;;    ;;;  
;   ;   ;  ;;  ;  ;; ;;   ;;  ;   ;           ;; ;;      ;    ;    ;;  ;  ;   ; 
;   ;      ;   ;  ;   ;   ;       ;           ;   ;      ;    ;    ;   ;; ;     
;    ;;;   ;   ;  ;   ;   ;       ;           ;   ;   ;;;;    ;    ;;;;;;  ;;;  
;       ;  ;   ;  ;   ;   ;       ;           ;   ;  ;   ;    ;    ;          ; 
;   ;   ;  ;   ;  ;; ;;   ;       ;           ;; ;;  ;   ;    ;    ;      ;   ; 
;    ;;;   ;   ;   ;;;    ;       ;;;          ;;;;   ;;;;    ;;;   ;;;;   ;;;  
;                                                                               
;                                                                               
;                                                                               

#; {Date Date -> Boolean}
(define (<-short-date d1 d2)
  (define y1 (short-date-year d1))
  (define y2 (short-date-year d2))
  (cond
    [(and (<= y1 90) (<= y2 90)) (or (< y1 y2) (and (= y1 y2) (<-month-day d1 d2)))]
    [(and (<= y1 90) (>= y2 90)) #false]
    [(and (>= y1 90) (>= y2 90)) (or (> y1 y2) (and (= y1 y2) (<-month-day d1 d2)))]
    [(and (>= y1 90) (<= y2 90)) #true]))

#; {Date Date _> Boolean}
(define (<-month-day d1 d2)
  (define m1 (short-date-month d1))
  (define m2 (short-date-month d2))
  (or (> m1 m2)
      (and (= m1 m2) (<= (short-date-day d1) (short-date-day d2)))))

(module+ test
  (check-false (<-short-date (filename->date d1) (filename->date d2)))
  (check-true (<-short-date (filename->date d2) (filename->date d1)))
  (check-true (<-short-date (filename->date d2) (filename->date d3)))
  (check-true (<-short-date (filename->date d4) (filename->date d2)))

  (check-true (<-short-date (filename->date d5) (filename->date d2)))
  (check-false (<-short-date (filename->date d2) (filename->date d5)))

  (check-true (<-short-date (filename->date d5) (filename->date d6))))