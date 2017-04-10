
(define (mk-wire) (cons 'wire '()))

(define (mk-3-way-connector wire1 wire2 wire3)
   (list 'connection wire1 wire2 wire3))

(define (mk-battery wire+ wire- voltage)
   (list 'battery voltage wire+ wire-))

(define (mk-resistor wire+ wire- resistance)
   (list 'resistor resistance wire+ wire-))

(define (get-signal wire) 0)
(define (set-signal! wire) 0)
(define (add-action! wire) 0) ;run action when value changes

#| simple
(define a (mk-wire))
(define b (mk-wire))
(define battery (mk-battery a b 12))
(define res (mk-resistor a b 0.5))
|#

#| series
(define w-b+ (mk-wire))
(define w-b- (mk-wire))

(define w-r1-r2 (mk-wire))

(define battery (mk-battery w-b+ w-b- 12))
(define r1 (mk-resistor w-b+ w-r1-r2 0.5))
(define r2 (mk-resistor w-r1-r2 w-b- 1))
|#

#| parallel
(define w-b+ (mk-wire))
(define w-b- (mk-wire))
(define battery (mk-battery w-b+ w-b- 12))

(define w-r1+ (mk-wire))
(define w-r1- (mk-wire))
(define r1 (mk-resistor w-r1+ w-r1- 5))

(define w-r2+ (mk-wire))
(define w-r2- (mk-wire))
(define r2 (mk-resistor w-r2+ w-r2- 50))

(define con1 (mk-3-way-connector w-b+ w-r1+ w-r2))
(define con2 (mk-3-way-connector w-b- w-r1- w-r2-))

|#




(printf "test\n")
