#lang racket

;; process a single company/etf/mutual fund (base, capital gains, etc)

(provide
 sum-accounts/c
 sum-security/c

 (contract-out
  (struct assessment
    ([accounts (cons/c real? accounts/c)]
     [stocks   (cons/c real? (listof 1assessment?))]
     [mutuals  (cons/c real? (listof 1assessment?))]
     [etfs     (cons/c real? (listof 1assessment?))]
     [iras     (cons/c real? (listof 1assessment?))]
     [seps     (cons/c real? (listof 1assessment?))]))

  (struct 1assessment
    ([name    string?]
     [share#  natural?]
     [cost    real?]
     [current real?]
     [profit  real?]
     [base    real?]
     [capital-gains real?]))

  [assess-base-profit-value
   (->i ([security* (listof 1security/c)]
         [action    (-> string? string? (or/c string? real?))])
        (#:cache
         (cache (security*)
                ;; contract error neg pos: I used `(listof string?)` instead of `(listof (list/c ..))
                (-> (listof (list/c string? string?))
                    (cache/c (map first (all-syms+names security*))))))
        (r sum-security/c))]

  [assess-accounts
   (->i ([a accounts/c]) (r (a) (and/c sum-accounts/c (cons/c real? (curry equal? a)))))]))

(module+ examples
  (provide (all-defined-out)))

;; ---------------------------------------------------------------------------------------------------
(require Xbalanced/private/quotes)
(require Xbalanced/private/representations)

(module+ examples
  (require (submod Xbalanced/private/representations examples)))
(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Xbalanced/private/representations examples))
  (require rackunit))

;                              
;       ;                      
;       ;           ;          
;       ;           ;          
;    ;;;;  ;;;;   ;;;;;  ;;;;  
;   ;; ;;      ;    ;        ; 
;   ;   ;      ;    ;        ; 
;   ;   ;   ;;;;    ;     ;;;; 
;   ;   ;  ;   ;    ;    ;   ; 
;   ;; ;;  ;   ;    ;    ;   ; 
;    ;;;;   ;;;;    ;;;   ;;;; 
;                              
;                              
;                              

(struct assessment (accounts stocks mutuals etfs iras seps) #:transparent)

(struct 1assessment (name share# cost current profit base capital-gains) #:transparent)

(define sum-accounts/c (cons/c real? accounts/c))

(define sum-security/c (cons/c real? [listof 1assessment?]))

(module+ examples
  (define ass1 (1assessment n1 4 3 40.0 37.0 6 34.0))
  (define ass2 (1assessment nT 4 3 40.0 37.0 6 34.0))
  (define ass* (list ass1 ass2))
  (define assS (cons 40.0 (list ass1)))
  (define assT (cons 80.0 ass*))

  (define a2-ass (cons 1.0 (take a2 2)))

  (define complete-ass (assessment a2-ass assS assS assS assS assS)))

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

#;{[Listof Security] (String -> Price) #:cache (-> [Listof String] Cache)
                     -> (Cons Real [Listof Assessment])}
(define (assess-base-profit-value security* action #:cache (mk-cache (λ _ (hash))))
  (define assessment* (assess-all security* action mk-cache))
  (define total       (sum 1assessment-current assessment*))
  (cons total assessment*))

#;{[Listof Security] (String -> Price) Cache [Listof [List String String]] -> [Listof Assessment]}
(define (assess-all security* action mk-cache)
  (define sym*  (all-syms+names security*))
  (define cache (mk-cache sym*))
  (for/list ([s security*] [s+n sym*])
    [match-define (cons name-line records) s]
    (assess s (get-value s+n action cache))))

(define (assess s current-price)
  [match-define (cons name l0) s]
  (let company ([l l0] [tax-base 0] [cost 0] [shares# 0])
    (cond
      [(empty? l)
       (define current-value (* shares# current-price))
       (define profit        (- current-value cost))
       (define capital-gains (- current-value tax-base))
       (1assessment name shares# cost current-value profit tax-base capital-gains)]
      [else
       (define record    (first l))
       [define raw-cost  (record-cost record)]
       (define cost+     (if (record-dividend? record) cost (+ cost raw-cost)))
       (define tax-base+ (+ tax-base raw-cost))
       (company (rest l) tax-base+ cost+ (+ shares# (record-shares# record)))])))

(module+ test
  (check-equal? (assess s1 10.0) ass1)
  (check-equal? (assess s2 10.0) ass2)

  (check-within (assess-all s* (λ (x c) 10) (λ _ (hash "S" 10))) ass* .1)
  (check-within (assess-all s* (λ (x c) 10) (λ _ (hash))) ass* .1)
  (check-within (assess-base-profit-value s* (λ (x c) 10)) (cons 80.0 ass*) .1)

  (check-within (assess-base-profit-value s* (λ (x c) 10)) (cons 80.0 ass*) .1)
  (check-within (assess-base-profit-value s* (λ _ 10) #:cache (λ _ (hash "S" 10))) assT .1))

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

(define (assess-accounts accounts)
  (cons (sum identity (filter number? accounts)) accounts))

(module+ test
  (define a1 '("A" 1 "B" 2 "C" 3))
  (check-equal? (assess-accounts a1) (cons 6 a1))
  (check-equal? (assess-accounts a2) (cons 3.0 a2)))

;                       
;                       
;                       
;                       
;   ;;;;   ;   ;  ;   ; 
;       ;  ;   ;   ; ;  
;       ;  ;   ;   ;;;  
;    ;;;;  ;   ;    ;   
;   ;   ;  ;   ;   ;;;  
;   ;   ;  ;   ;   ; ;  
;    ;;;;   ;;;;  ;   ; 
;                       
;                       
;                       

#; {[X] (X -> Real) [Listof X] -> Real}
(define (sum f records)
  (apply + (map f records)))

(module+ test
  (check-equal? (sum identity '(1 2 3 4)) 10))

