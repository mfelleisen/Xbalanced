#lang racket

;; This module implements an extremely simple stock and fund quote service.
;; WARNING Running this module in DrRacke runs network-based tests. 

(define price? (and/c real? positive?))
(define result/c (or/c price? string?))
(define (cache/c sym*)
  (and/c (hash/c string? price?) (λ (h) (subset? (hash-map h (λ (x _) x)) sym*))))

(provide
 cache/c
 price?

 (contract-out
  [stock-quote*
   #; (stock-quote* los api key) ; retrieves the prices of all stock symbols on `los`
   ;; to keep the retrieval inexpensive; the result is a cache 
   (->i ([s+n* (listof (list/c string? string?))] [api string?] [key string?])
        (r (s+n*) (cache/c (map first s+n*))))]
  
  [lookup
   #; (lookup cache sym name api) ; retrieves the price from te cache
   ;; or returns a string that explains which price wasn't found 
   (-> cache/c string? string? result/c)]

  [fund-quote
   #; (fund-quote F api) ; retrieves the fund price of F from FUND-API
   ;; EFFECT this may raise interprocess communication exns
   (-> string? string? string? result/c)]
  
  [stock-quote
   #; (stock-quote S N api key) ; retrievs the stock price of S from STOCK-API
   ;; EFFECT this may raise interprocess communication exns
   (-> string? string? string? string? result/c)]
  
  [etf-quote
   #; (fund-quote F api) ; retrieves the fund price of S from FUND-API
   ;; EFFECT this may raise interprocess communication exns
   (-> string? string? string? result/c)]

  [get-value
   ;; retrieve the value of securities with 3 retries at most
   (-> (list/c string? string?) (-> string? string? (or/c real? string?)) cache/c real?)]))

;; ---------------------------------------------------------------------------------------------------
(require racket/hash)
(require net/uri-codec)
(require json)

;; ---------------------------------------------------------------------------------------------------
;; WORKAROUND for 9.2.0.5+
(require (only-in net/url string->url get-impure-port get-pure-port call/input-url))
(require net/url-structs)

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require (submod ".."))
  (require rackunit))

(define NOT-FOUND "'s price can't be retrieved with symbol ")

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

;; this protocol is set up for testing

(define original call/input-url)

(define *call/input-url original)

(define (set-call/input-url f) (set! *call/input-url f))

(define ([fake-call/input-url in] url which-port reader . headers)
  (with-input-from-string in reader))

;; ---------------------------------------------------------------------------------------------------
;                                                                                             
;      ;;                    ;                                               ;;               
;     ;                      ;                  ;;;                  ;      ;                 
;     ;                      ;                 ;                     ;      ;                 
;   ;;;;;  ;   ;  ; ;;    ;;;;   ;;;           ;             ;;;   ;;;;;  ;;;;;   ;;;         
;     ;    ;   ;  ;;  ;  ;; ;;  ;   ;          ;;           ;;  ;    ;      ;    ;   ;        
;     ;    ;   ;  ;   ;  ;   ;  ;              ;;           ;   ;;   ;      ;    ;            
;     ;    ;   ;  ;   ;  ;   ;   ;;;          ;  ; ;        ;;;;;;   ;      ;     ;;;         
;     ;    ;   ;  ;   ;  ;   ;      ;         ;  ;;;        ;        ;      ;        ;        
;     ;    ;   ;  ;   ;  ;; ;;  ;   ;         ;;  ;         ;        ;      ;    ;   ;        
;     ;     ;;;;  ;   ;   ;;;;   ;;;           ;;; ;         ;;;;    ;;;    ;     ;;;         
;                                                                                             
;                                                                                             
;                                                                                             

(define FUND-REG #px"<p>.*;(\\d+\\.\\d+)<span>")

(define ETF-REG  #px"<td>(\\d+\\.\\d+)</td>")

(define (fund-quote sym name fund-api)
  (define price (backup-retrieve fund-api FUND-REG sym))
  (or price (~a name NOT-FOUND sym)))

(define (etf-quote sym name etf-api)
  (define price (backup-retrieve etf-api ETF-REG sym))
  (or price (~a name NOT-FOUND sym)))

;; ---------------------------------------------------------------------------------------------------
#; {String RegExp StockSymbol -> Price}
(define (backup-retrieve api REG sym)
  (define lines (retrieve-raw-wev-page api sym))
  (retrieve-price-from-page REG lines))

#; {RegExp Stocksymbol -> Price}
(define (retrieve-raw-wev-page API sym)
  (parameterize ((current-alist-separator-mode 'amp))
    (define url:string (format API sym sym))
    (define url:url    (string->url url:string))
    (*call/input-url url:url get-pure-port port->lines)))

#; {String [Listof String] -> (U False Price)}
(define (retrieve-price-from-page REG lines)
  (for*/first ([line lines] [the-quote (in-value (regexp-match REG line))] #:when the-quote)
    (string->number (second the-quote))))

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

  (define test-api-mutual-etf "testing ~a ~a")

  (define etf-price1 "<td>10.0</td>")
  (define fund-price1 "<p>the price is;20.01<span>")
  
  (check-equal? (retrieve-price-from-page ETF-REG (list etf-price1)) 10.0)
  (check-false (retrieve-price-from-page ETF-REG (list "a" "b" "c")))

  (check-equal? (retrieve-price-from-page FUND-REG (list fund-price1)) 20.01))

(module+ test ;; these are a bit artifical 
  (dynamic-wind
   (λ () (set-call/input-url [fake-call/input-url (~a "a\n" etf-price1 "\nb")]))
   (λ () (check-equal? (retrieve-raw-wev-page test-api-mutual-etf 'ETF1) '("a" "<td>10.0</td>" "b")))
   (λ () (set-call/input-url original)))

  (dynamic-wind
   (λ () (set-call/input-url [fake-call/input-url (~a "a\n" etf-price1 "\nb")]))
   (λ () (check-equal? (backup-retrieve test-api-mutual-etf  ETF-REG "S") 10.0))
   (λ () (set-call/input-url original)))

  (dynamic-wind
   (λ () (set-call/input-url [fake-call/input-url (~a "a\n" etf-price1 "\nb")]))
   (λ () (check-equal? (etf-quote "S" "S Fund" test-api-mutual-etf) 10.0))
   (λ () (set-call/input-url original)))

  (dynamic-wind
   (λ () (set-call/input-url [fake-call/input-url (~a "a\n" "\nb")]))
   (λ () (check-equal? (etf-quote "S" "S Fund" test-api-mutual-etf)
                       "S Fund's price can't be retrieved with symbol S"))
   (λ () (set-call/input-url original)))

  (dynamic-wind
   (λ () (set-call/input-url [fake-call/input-url (~a "a\n" fund-price1 "\nb")]))
   (λ () (check-equal? (backup-retrieve test-api-mutual-etf  FUND-REG "S") 20.01))
   (λ () (set-call/input-url original)))
  
  (dynamic-wind
   (λ () (set-call/input-url [fake-call/input-url (~a "a\n" fund-price1 "\nb")]))
   (λ () (check-equal? (fund-quote "S" "S Fund" test-api-mutual-etf) 20.01))
   (λ () (set-call/input-url original)))

  (dynamic-wind
   (λ () (set-call/input-url [fake-call/input-url (~a "a\n" "\nb")]))
   (λ () (check-equal? (fund-quote "S" "S Fund" test-api-mutual-etf)
                       "S Fund's price can't be retrieved with symbol S"))
   (λ () (set-call/input-url original)))
  )

;                                            
;                               ;            
;            ;                  ;            
;            ;                  ;            
;    ;;;   ;;;;;   ;;;    ;;;   ;  ;    ;;;  
;   ;   ;    ;    ;; ;;  ;;  ;  ;  ;   ;   ; 
;   ;        ;    ;   ;  ;      ; ;    ;     
;    ;;;     ;    ;   ;  ;      ;;;     ;;;  
;       ;    ;    ;   ;  ;      ; ;        ; 
;   ;   ;    ;    ;; ;;  ;;     ;  ;   ;   ; 
;    ;;;     ;;;   ;;;    ;;;;  ;   ;   ;;;  
;                                            
;                                            
;                                            

(define STOCK-MAX 10) ;; this is hhow many prices can be retrieved per batch

; ---------------------------------------------------------------------------------------------------
#; {[List String String] (String ->  (U String Price)) Cache -> Price}
;; try N times to retrieve the value of the `sym` security either from cache or via `action`
(define (get-value sym+name action cache)
  [match-define (list sym name) sym+name]
  (define cached-price (lookup cache sym name))
  (let retry ([n 3] [actual-price (if (number? cached-price) cached-price (action sym name))])
    (cond
      [(number? actual-price) actual-price]
      [(< n 0)                (error 'get-value actual-price)]
      [else                   (retry (sub1 n) (action sym name))])))

;; ---------------------------------------------------------------------------------------------------
(define (stock-quote company company-name (api "") (key ""))
  [define pt (retrieve-table [list company] api key)]
  (lookup pt company company-name))

#; {type PriceTable = [Hashof SymbolString Real]}

#; {PriceTable SymbolString NameString -> Real u String}
(define (lookup pt company company-name)
  (hash-ref pt company (mk-error-msg company company-name)))

#; {String String -> String}
(define [(mk-error-msg company company-name)]
  (~a company-name NOT-FOUND company))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof [List SymbolString NameString]] 2FormatString String-> PriceTable}
(define (stock-quote* companies0 api key)
  ;; pt-all is a price-table for all but companies
  (let one-batch ([companies companies0] [pt-all (hash)])
    (cond
      [(<= (length companies) STOCK-MAX)
       (hash-union pt-all (retrieve-table (map first companies) api key)
                   #:combine
                   (λ (a b)
                     (if (equal? a b)
                         a
                         (error 'stock-quote* "two distinct values for same key: ~a ~a\n"  a b))))]
      [else
       (define next-batch (take companies STOCK-MAX))
       (define next-pt    (retrieve-table (map first next-batch) "" ""))
       (one-batch (drop companies STOCK-MAX) (hash-union pt-all next-pt))])))

#; {[Listof CompanyString] -> PriceTable}
;; https://api.iextrading.com/1.0/tops/last?symbols=SNAP,fb,AIG%2b
(define/contract (retrieve-table stock-symbol* STOCK-API STOCK-KEY)
  (->i ([sym* [listof string?]] [api string?] [key string?])
       (r (sym*) (cache/c sym*)))
  (define url:string (string-append STOCK-API (string-join stock-symbol* ",")))
  (define url:url    (string->url url:string))
  (define header     `[ ,(~a "x-api-key: " STOCK-KEY) ])
  (define response   (*call/input-url url:url get-pure-port read-json header))
  (jsexpr->price-table response))

#; {JSexpr -> PriceTable}
(define (jsexpr->price-table j)
  (define response (hash-ref j 'quoteResponse))
  (define result   (hash-ref response 'result))
  (unless (null? (hash-ref response 'error))
    (log-error (~a "log error code is: " (hash-ref response 'error))))
  (for/hash ([h result])
    (values (hash-ref h 'symbol) (hash-ref h 'regularMarketPrice))))

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
  (check-equal? (get-value '("C" "Company") (λ _ 30.0) (hash)) 30.0)
  (check-equal? (get-value '("C" "Company") (λ _ "cannot be covered") (hash "C" 30.0)) 30.0)
  (check-exn exn:fail? (λ () (check-equal? (get-value '("C" "Company") (λ _ "no") (hash))))))

(module+ test
  (define response
    (hash 'quoteResponse
          (hash 'error '()
                'result (list (hash 'symbol "S" 'regularMarketPrice 40.10)))))

  (define expected (hash "S" 40.1))

  (check-equal? (jsexpr->price-table response) expected))

(module+ test ;; these are a bit fake again

  (dynamic-wind
   (λ () (set-call/input-url [fake-call/input-url (jsexpr->string response)]))
   (λ () (check-equal? (retrieve-table '("S") "" "") expected))
   (λ () (set-call/input-url original)))

  (define fake-list (make-list (* 2 STOCK-MAX) '("S" "Company")))
  
  (dynamic-wind
   (λ () (set-call/input-url [fake-call/input-url (jsexpr->string response)]))
   (λ () (check-equal? (stock-quote* fake-list "~a~a" "") expected))
   (λ () (set-call/input-url original)))

  (dynamic-wind
   (λ () (set-call/input-url [fake-call/input-url (jsexpr->string response)]))
   (λ () (check-equal? (stock-quote "S" "Company" "~a ~a" "test key") 40.1))
   (λ () (set-call/input-url original))))
