#lang racket

;; render and print an assessment as an S-expression

(provide
 (contract-out
  [render-as-sexpr (-> assessment? any)]))

(module+ examples
  (provide (all-defined-out)))

;; ---------------------------------------------------------------------------------------------------
(require Xbalanced/private/assessment)
(require Xbalanced/private/representations)
(require Xbalanced/private/render-common)

(module+ examples
  (require (submod Xbalanced/private/representations examples)))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Xbalanced/private/assessment examples))
  (require rackunit))

;                                            
;                 ;                          
;   ;;;           ;             ;;;          
;     ;           ;               ;          
;     ;    ;;;;   ;;;;    ;;;     ;     ;;;  
;     ;        ;  ;; ;;  ;;  ;    ;    ;   ; 
;     ;        ;  ;   ;  ;   ;;   ;    ;     
;     ;     ;;;;  ;   ;  ;;;;;;   ;     ;;;  
;     ;    ;   ;  ;   ;  ;        ;        ; 
;     ;    ;   ;  ;; ;;  ;        ;    ;   ; 
;      ;;   ;;;;  ;;;;    ;;;;     ;;   ;;;  
;                                            
;                                            
;                                            

(define LENGTH-OF-LINE 60)
(define LENGTH-OF-NAME 20)
(define INDENT-SUM (+  LENGTH-OF-NAME 20))
(define DELTA 10)

(define -line ";----------------------------------------------------------------------")
(define =line ";======================================================================")

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
  (define width-a2 (+ DELTA LENGTH-OF-NAME DELTA -1 DELTA))
  (define a2-ass-expected
    (~a (first a2)
        (->fixed-width-text 'right width-a2 (render-amounts-of-pennies (second a2)))
        "\n"
        (->bottom -line 1.0)))

  (define ass1-expected1 "(S) Stock1                3.00      6.00     40.00     37.00")
  
  (define (assS-expected (s STOCKs))
    (~a (->header s) "\n"
        (~a "(" ass1-expected1 ")\n")
        (->bottom -line 40)))

  (define complete-ass-exp
    (~a (paren-it (assS-expected STOCKs) )
        (paren-it (assS-expected MUTUALs) )
        (paren-it (assS-expected ETFs) )
        (paren-it a2-ass-expected)
        (format "~a\n" (->bottom =line 121))
        (paren-it (assS-expected IRAs) )
        (paren-it (assS-expected SEPs) ))))

;                                            
;                            ;               
;                            ;               
;                            ;               
;    ;;;;   ;;;   ; ;;    ;;;;   ;;;    ;;;; 
;    ;;  ; ;;  ;  ;;  ;  ;; ;;  ;;  ;   ;;  ;
;    ;     ;   ;; ;   ;  ;   ;  ;   ;;  ;    
;    ;     ;;;;;; ;   ;  ;   ;  ;;;;;;  ;    
;    ;     ;      ;   ;  ;   ;  ;       ;    
;    ;     ;      ;   ;  ;; ;;  ;       ;    
;    ;      ;;;;  ;   ;   ;;;;   ;;;;   ;    
;                                            
;                                            
;                                            

;; Compute Monthly BALANCE
(define [(render-as-sexpr assess*)]
  (printf "~a\n" (->as-sexpr assess*)))

(define (->as-sexpr assess*)
  (match-define (assessment sum-accounts sum-stocks sum-funds sum-etfs sum-m-iras sum-m-sep) assess*)
  (define total (+ (first sum-stocks) (first sum-funds) (first sum-etfs) (first sum-accounts)))

  ;; --- non-retirement
  (~a
   (paren-it (->one-kind-of-security sum-stocks STOCKs))
   (paren-it (->one-kind-of-security sum-funds  MUTUALs))
   (paren-it (->one-kind-of-security sum-etfs   ETFs))
   (paren-it (->cash-accounts sum-accounts))
  
   ;; --- the bottom line value of all non-ira values ---
   (format "~a\n" (->bottom =line total))
  
   ;; --- the retirement accounts 
   (paren-it (->one-kind-of-security sum-m-iras IRAs))
   (paren-it (->one-kind-of-security sum-m-sep SEPs))))

(define fmt "~n(~n~a\n)\n")

#; {String -> String}
(define (paren-it txt)
  (format fmt txt))

(module+ test
  (check-equal? (->as-sexpr complete-ass) complete-ass-exp)
  (check-equal? (with-output-to-string (render-as-sexpr complete-ass)) (~a complete-ass-exp "\n")))

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

(define (->cash-accounts value+accounts)
  (match-define (cons total accounts) value+accounts)
  (define accounts*
    (let loop ((accounts accounts))
      (cond
        [(empty? accounts) '()]
        [else
         (cons (->account (first accounts) (second accounts)) 
               (loop (rest (rest accounts))))])))
  (define bottom (->bottom -line total))

  (~a (string-join accounts* "\n") "\n" bottom))

(define (->account name bal)
  (~a (->fixed-width-text 'left (+ LENGTH-OF-NAME 1 #;parens (* 2 DELTA) #;columns) name)
      (->fixed-width-text 'right DELTA (render-amounts-of-pennies bal))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (->cash-accounts a2-ass) a2-ass-expected))
  

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

(define (->one-kind-of-security value+assessment* hdr)
  (match-define [cons sum assessment*] value+assessment*)
  (string-join `(,(->header hdr) ,@(->assessment* assessment*) ,(->bottom -line sum)) "\n"))

(define (->assessment* assessment*)
  (for/list ([a assessment*]) (~a "(" (->assessment a) ")")))
  
(define (->assessment a)
  (string-join
   (list 
    (->fixed-width-text 'left  LENGTH-OF-NAME (1assessment-name a))
    (->fixed-width-text 'right DELTA (render-amounts-of-pennies (1assessment-cost a)))
    (->fixed-width-text 'right DELTA (render-amounts-of-pennies (1assessment-base a)))
    (->fixed-width-text 'right DELTA (render-amounts-of-pennies (1assessment-current a)))
    (->fixed-width-text 'right DELTA (render-amounts-of-pennies (1assessment-profit a))))
   ""))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (->assessment ass1) ass1-expected1)
  (check-equal? (->assessment* (list ass1)) (list (~a "(" ass1-expected1 ")")))

  (check-equal? (->one-kind-of-security assS STOCKs) (assS-expected )))

;                                                   
;      ;;                                           
;     ;                            ;                
;     ;                                             
;   ;;;;;   ;;;;  ;;;;  ;;;;;;   ;;;   ; ;;    ;;;; 
;     ;     ;;  ;     ; ;  ;  ;    ;   ;;  ;  ;;  ; 
;     ;     ;         ; ;  ;  ;    ;   ;   ;  ;   ; 
;     ;     ;      ;;;; ;  ;  ;    ;   ;   ;  ;   ; 
;     ;     ;     ;   ; ;  ;  ;    ;   ;   ;  ;   ; 
;     ;     ;     ;   ; ;  ;  ;    ;   ;   ;  ;; ;; 
;     ;     ;      ;;;; ;  ;  ;  ;;;;; ;   ;   ;;;; 
;                                                 ; 
;                                              ;  ; 
;                                               ;;  

(define (->header title)
  (define lft (~a "; " title))
  (~a (~a lft #:min-width (- LENGTH-OF-LINE (string-length HEADER))) HEADER))

(define (->bottom line sum)
  (~a line "\n"
      (->fixed-width-text 'left INDENT-SUM SUM)
      (->fixed-width-text 'right 11 (render-amounts-of-pennies sum))))

(define (->fixed-width-text LorR N text)
  (define L (string-length text))
  (~a (substring text 0 (min L (- N 1))) #:min-width N #:max-width N #:align LorR))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; fixed width 
  (define 3as (make-string 3 #\a))
  (check-equal? (->fixed-width-text 'left  4 3as) "aaa ")
  (check-equal? (->fixed-width-text 'right 4 3as) " aaa"))

(module+ test ;; bottom 
  (check-equal? (->bottom =line 100.1)
                (~a =line
                    "\n"
                    (->fixed-width-text  'left INDENT-SUM SUM)
                    (->fixed-width-text  'right 11 "100.10"))))

(module+ test
  (check-equal? (->header STOCKs) (~a "; " STOCKs "                 " HEADER)))
