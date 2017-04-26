
;(define (mk-wire) (cons 'wire '()))

(define (call-each f-lst arg)
   (if (null? f-lst)
      'done
      (begin ((car f-lst) arg)
             (call-each (cdr f-lst) arg))))

(define (cons-end-list lst item)
   (if (null? lst)
      (cons item lst)
      (cons (car lst) (cons-end-list (cdr lst) item))))

(define (rm-last-item lst)
   (if (pair? lst)
      (cdr lst)
      (cons (car lst) (rm-last-item (cdr lst)))))



(define (get-signal wire)
   (wire 'get-signal))
(define (set-signal! wire new-value)
   ((wire 'set-signal!) new-value))
(define (add-action! wire on-change) ;run action when value changes
   ((wire 'add-action!) on-change))


;;agenda
(define (mk-agenda)
   '())

(define (empty-agenda? agenda)
   (null? agenda))

(define (first-agenda-item agenda)
   (car agenda))

(define (remove-first-agenda-item! agenda)
   (set! agenda (cdr agenda)))

(define (add-to-agenda! agenda time action)
   (set! agenda (cons-end-list action agenda)))
   ;(list (car agenda) action)))

;(define (current-time agenda) 0)

;;

(define the-agenda (mk-agenda))

;(define (after-delay delay action)
;   (add-to-agenda! (+ delay (current-time the-agenda))
;                   action the-agenda))

(define (propagate)
   (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
         (first-item)
         (remove-first-agenda-item! the-agenda)
         (propogate))))


(define (mk-wire)
   (let ((my-amp 0) (my-volt 0) (on-change '()))
      (define (set-my-amp! new-my-amp)
         (if (not (= my-amp new-my-amp))
            (begin (set! my-amp new-my-amp)
                   (call-each on-change new-my-amp))
            'done))
      (define (accept-on-change! proc)
         (set! on-change (cons proc on-change))
         (proc))
      (define (dispatch m)
         (cond ((eq? m 'get-signal) my-amp)
               ((eq? m 'set-signal!) set-my-signal!)
               ((eq? m 'add-action!) accept-on-change!)
               (else (error "Uknown wire command" m))))
      dispatch))


(define (mk-3-way-connector wire1 wire2 wire3)
   (set-signal! wire-2 (get-signal wire1))
   (set-signal! wire3 (get-signal wire1)))
;(list 'connection wire1 wire2 wire3)

(define (mk-battery wire+ wire- voltage)
   (list 'battery voltage wire+ wire-))

(define (mk-resistor wire+ wire- resistance)
   (list 'resistor resistance wire+ wire-))

;;tests


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
