#lang racket

(provide
 (contract-out
  [render-as-html (-> assessment? any)]))

;; ---------------------------------------------------------------------------------------------------
(require Xbalanced/private/assessment)
(require Xbalanced/private/representations)
(require Xbalanced/private/render-common)

(module+ test
  (require (submod ".."))
  (require (submod Xbalanced/private/assessment examples))
  (require (submod Xbalanced/private/representations examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (render-as-html a*)
  (page-template 
   (sum+security->section STOCKs   (assessment-stocks a*)   1assessment->stock%)
   (sum+security->section MUTUALs  (assessment-mutuals a*)  1assessment->stock%)
   (sum+security->section ETFs     (assessment-etfs a*)     1assessment->stock%)
   (sum+security->section ACCOUNTs (assessment-accounts a*) accounts->account%)
   (sum+security->section IRAs     (assessment-iras a*)     1assessment->stock%)
   (sum+security->section SEPs     (assessment-seps a*)     1assessment->stock%)))

#; {String  (Cons/c Real X) [X -> [Listof Sectrity]] -> Xexpr}
(define (sum+security->section header a a->security)
  (match-define (cons sum 1assessment-or-account) a)
  (define security* (a->security 1assessment-or-account))
  (define table     (securities->table-body security*))
  (define sum:r     (render-amounts-of-pennies sum))
  (table-template header sum:r table))

(require SwDev/Debugging/diff)

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

;; the conversion to classes (is historical and) allows a uniform treatment of all securities
;; (yes the word security isn't quite right because accounts are totally different)

#; {[Listof Assessment] -> [Listof (Instanceof Stock%)]}
(define (1assessment->stock% a*)
  (map (λ (s) (new Stock% [s s])) a*))

#; {Accounts/c -> [Listof (Instanceof Account%)]}
;; create name-value pairs from ... symbol1 symbol2 symbol3 number ... in list 
(define (accounts->account% a*)
  (let loop ([a a* #;(rest (rest a*))])
    (match a
      ['() '()]
      [(list* (? string? name) (? real? value) others)
       (define a:account (new Account% [s (list name value)]))
       (cons a:account (loop (rest (rest a))))])))

(define Security%
  (class object% (init-field s)
    (super-new)))
  
(define Account%
  (class Security% (inherit-field s)
    (super-new)

    (define name  (first s))
    (define value (second s))
    (define/public (render-as-row color:String) 
      (row-of-width-5 color:String name "" "" (render-amounts-of-pennies value) ""))))
  
(define Stock%
  (class Security% (inherit-field s)
    (super-new)
    
    (define name
      (1assessment-name s))
    (define cost-base-current-profit
      (list (1assessment-cost s) (1assessment-base s) (1assessment-current s) (1assessment-profit s)))
    (define/public (render-as-row color) 
      (apply row-of-width-5 color name (map render-amounts-of-pennies cost-base-current-profit)))))

;                              
;   ;                          
;   ;        ;           ;;;   
;   ;        ;             ;   
;   ; ;;   ;;;;; ;;;;;;    ;   
;   ;;  ;    ;   ;  ;  ;   ;   
;   ;   ;    ;   ;  ;  ;   ;   
;   ;   ;    ;   ;  ;  ;   ;   
;   ;   ;    ;   ;  ;  ;   ;   
;   ;   ;    ;   ;  ;  ;   ;   
;   ;   ;    ;;; ;  ;  ;    ;; 
;                              
;                              
;                              

#; {Xexpr ... -> Xexpr} 
(define (page-template . section*)
  `(html 
    (head (title ,HOLDINGs))
    (body
     (a ([href ,INDEX.html]) ,INDEX)
     ,(head)
     ,@section*)))

(define (head)
  `(p
    (table 
     ,(apply row-of-width-5
       "white"
       WHAT
       (string-split HEADER) ;"Cost" "Base" "Current" "Profit"
       #:cellalign:String "left"
       #:cellkind:Symbol 'th))))

#; {String String Xexpr -> Xerpr}
(define (table-template header sum table-body)
  `(p (hr)
      (table
       [(border "0")]
       (tr ((valign "top"))
           (td
            (table ((border "0"))
                   ,@table-body
                   ,(row-of-width-5 "pink" "" "" "" sum "")))
           (td (h4 ,header))))))

#; {[Listof (Instanceof Security%)] -> [Listof Xexpr]}
(define (securities->table-body security*)
  (define default-color "lightblue")
  (define other-color   "lightgray")
  (for/fold ([r '()] [c default-color] #:result (reverse r)) ([s security*])
    (values
     (cons (send s render-as-row c) r)
     (if (eq? c default-color) other-color default-color))))

#; {String String String String String String -> Xexpr}
;; optional: cellalign:String, cellkind:Symbol
;; create a row for a stock or account table 
(define (row-of-width-5
         bg:String
         c1:String c2:String c3:String c4:String c5:String
         #:cellalign:String (align "right")
         #:cellkind:Symbol  (kind 'td))
  `(tr ((bgcolor ,bg:String)) 
       (,kind ((align "left")  (width "200")) ,c1:String)
       (,kind ((align ,align) (width "100")) ,c2:String)
       (,kind ((align ,align) (width "100")) ,c3:String)
       (,kind ((align ,align) (width "100")) ,c4:String)
       (,kind ((align ,align) (width "100")) ,c5:String)))

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
  (define (is5-row? r)
    (= (length (filter (λ (x) (eq? (car x) 'td)) (rest r))) 5))

  (define table-body-for-ass1
    `((tr
       ((bgcolor "lightblue"))
       (td ((align "left") (width "200")) ,n1)
       (td ((align "right") (width "100")) "3.00")
       (td ((align "right") (width "100")) "6.00")
       (td ((align "right") (width "100")) "40.00")
       (td ((align "right") (width "100")) "37.00"))))

  (define (full-table-for-ass1 label)
    `(p
      (hr)
      (table
       ((border "0"))
       (tr
        ((valign "top"))
        (td
         (table
          ((border "0"))
          ,@table-body-for-ass1
          (tr
           ((bgcolor "pink"))
           (td ((align "left") (width "200")) "")
           (td ((align "right") (width "100")) "")
           (td ((align "right") (width "100")) "")
           (td ((align "right") (width "100")) ,(render-amounts-of-pennies (first assS)))
           (td ((align "right") (width "100")) ""))))
        (td (h4 ,label))))))

  (define full-page-for-ass1
    `(html
      (head (title ,HOLDINGs))
      (body
       (a ((href "index.html")) "The Index")
       (p
        (table
         (tr
          ((bgcolor "white"))
          (th ((align "left") (width "200")) ,WHAT)
          (th ((align "left") (width "100")) ,COST)
          (th ((align "left") (width "100")) ,BASE)
          (th ((align "left") (width "100")) ,CURRENT)
          (th ((align "left") (width "100")) ,PROFIT))))
       ,(full-table-for-ass1 STOCKs))))

  (define table-body-for-a2
    '((tr
       ((bgcolor "lightblue"))
       (td ((align "left") (width "200")) "C1")
       (td ((align "right") (width "100")) "")
       (td ((align "right") (width "100")) "")
       (td ((align "right") (width "100")) "1.00")
       (td ((align "right") (width "100")) ""))))

  (define full-table-body-for-a2
    `(p
      (hr)
      (table
       ((border "0"))
       (tr
        ((valign "top"))
        (td
         (table
          ((border "0"))
          ,@table-body-for-a2
          (tr
           ((bgcolor "pink"))
           (td ((align "left") (width "200")) "")
           (td ((align "right") (width "100")) "")
           (td ((align "right") (width "100")) "")
           (td ((align "right") (width "100")) ,(render-amounts-of-pennies (first a2-ass)))
           (td ((align "right") (width "100")) ""))))
        (td (h4  ,ACCOUNTs)))))))

(module+ test
  (check-true (is5-row? (row-of-width-5 "pink" "1" "2" "3" "4" "5")))

  (check-true (andmap is5-row? (securities->table-body (1assessment->stock% ass*))))
  
  (check-equal? (securities->table-body (1assessment->stock% (list ass1)))
                table-body-for-ass1)
  
  (check-equal? (sum+security->section STOCKs assS 1assessment->stock%)
                (full-table-for-ass1 STOCKs))

  (check-equal? (page-template (sum+security->section STOCKs assS 1assessment->stock%))
                full-page-for-ass1))

(module+ test
  (check-true (andmap is5-row? (securities->table-body (accounts->account% a2))))
  
  (check-equal? (securities->table-body (accounts->account% (take a2 2))) table-body-for-a2)
  
  (check-equal? (sum+security->section ACCOUNTs a2-ass accounts->account%) full-table-body-for-a2))

(module+ test
  (check-equal? (render-as-html complete-ass)
                (page-template
                 (full-table-for-ass1 STOCKs)
                 (full-table-for-ass1 MUTUALs)
                 (full-table-for-ass1 ETFs)
                 (sum+security->section ACCOUNTs a2-ass accounts->account%)
                 (full-table-for-ass1 IRAs)
                 (full-table-for-ass1 SEPs))))
