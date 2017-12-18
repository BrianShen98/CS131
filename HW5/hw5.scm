#lang racket
(define (null-ld? obj) (cond [(not(pair? obj)) #f]
			     [(eq? (car obj) (cdr obj)) #t]
			     [else #f]))

(define (listdiff? obj)
  (cond [(not(pair? obj))#f];empty list case
	[(eq? (car obj) (cdr obj)) #t]
	[(not(pair? (car obj))) #f]; car(obj) is '() result from a proper list or a symbol result from an improper list
	[else (listdiff? (cons (cdr(car obj)) (cdr obj)))]))

(define (cons-ld obj listdiff) (if (listdiff? listdiff) (cons (cons obj (car listdiff)) (cdr listdiff)) (display "error: second arg is not a listdiff!\n")))

(define (car-ld listdiff) (cond [(not(listdiff? listdiff)) (display "error: not a listdiff!\n")]
				[(null-ld? listdiff) (display "error: empty listdiff!\n")]
				[else (car(car listdiff))])) 

(define (cdr-ld listdiff) (cond [(not (listdiff? listdiff)) (display "error: not a listdiff!\n")]
				[(null-ld? listdiff) (display "error: empty listdiff!\n")]
				[else (cons (cdr(car listdiff)) (cdr listdiff))]
				))

(define listdiff (lambda obj (cons obj '())))

(define (length-ld listdiff) (cond [(not(listdiff? listdiff)) (display "error: not a listdiff!\n")]
				   [(null-ld? listdiff) 0]
				   [else (+ 1 (length-ld (cdr-ld listdiff)))]))


;;(define uni_append (lambda (x) (cond [(empty?(cdr x)) '()]
;;				     [(null-ld? (car x)) (uni_append (cdr x))]
;;				     [else (cons (car-ld (car x)) (uni_append (cons (cdr-ld (car x)) (cdr x))) )])))

(define append-ld (lambda listdiffs (cond [(empty? listdiffs) '(())]
					  [else (letrec ([lastdiff (car(reverse listdiffs))] [uni_append (lambda (x) (cond [(empty?(cdr x)) '()]
															   [(null-ld? (car x)) (uni_append (cdr x))]
															   [else (cons (car-ld (car x)) (uni_append (cons (cdr-ld (car x)) (cdr x))) )]))] ) (cons (append (uni_append listdiffs) (car lastdiff)) (cdr lastdiff) ))])))


(define (list-tail-ld listdiff k) (cond [(= k 0) listdiff]
					[(< k 0) (display "error: k is negative!\n")]
					[(> k (length-ld listdiff)) (display "error: k exceeds the length of listdiff!\n")]
					[else (list-tail-ld (cdr-ld listdiff) (- k 1))]))


(define (list->listdiff list) (cond [(not(list? list)) (display "error: argument not a list!\n")]
				     [else (cons list '())]))

(define (listdiff->list listdiff)(cond [(not(listdiff? listdiff)) (display "error: argument not a listdiff!\n")]
				       [(null-ld? listdiff) '()]
				       [else (cons (car-ld listdiff) (listdiff->list (cdr-ld listdiff)))]) )


;(define (construct test) (cond [(not(pair? test)) (quasiquote (quote(unquote test)))]
;			    [else (quasiquote(cons (quote(unquote(car test))) (unquote (construct (cdr test)))))]))

;(define (cndr listdiffs front) (cond [(equal? (car listdiffs) front) (display listdiffs)]
;				     [else (write front)]))
;(define (expr-returning listdiffs) (letrec ([construct (lambda (x) (cond [(not(pair? x)) (quasiquote (quote(unquote x)))]
;									 [else (quasiquote(cons (quote(unquote(car x))) (unquote (construct (cdr x)))))]))][front (construct (car listdiffs))]) (quasiquote(cons (unquote front) (quote(unquote (cndr listdiffs front) ))))) )

(define (expr-returning listdiff)
  (define (generate-symbols list)
    (if (null? list)
        (quasiquote tail)
        (let ((inner (generate-symbols (cdr list))))
          (quasiquote (cons (quote (unquote (car list))) (unquote inner))))))
  (if (null-ld? listdiff)
      (quasiquote (let ((last-el '(1)))
                    (cons last-el last-el)))
      (let ((list (listdiff->list listdiff)))  
        (quasiquote (let ((tail (quote (unquote (cdr listdiff)))))
                      (cons (unquote (generate-symbols list)) tail))))))

;; (define ils (append '(a e i o u) 'y))
;; (define d1 (cons ils (cdr (cdr ils))))
;; (define d2 (cons ils ils))
;; (define d3 (cons ils (append '(a e i o u) 'y)))
;; (define d4 (cons '() ils))
;; (define d5 0)
;; (define d6 (listdiff ils d1 37))
;; (define d7 (append-ld d1 d2 d6))
;; (define e1 (expr-returning d1))


;; (listdiff? d1)
;; (listdiff? d2)
;; (listdiff? d3)
;; (listdiff? d4)
;; (listdiff? d5)                        
;; (listdiff? d6)
;; (listdiff? d7)

;; (null-ld? d1)
;; (null-ld? d2)
;; (null-ld? d3)
;; (null-ld? d6)

;; (car-ld d1)
;; (car-ld d2)
;; (car-ld d3)
;; (car-ld d6)

;; (cdr-ld d1)
;; (cdr-ld d2)
;; (cdr-ld d3)
;; (cdr-ld d6)

;; (listdiff 1 2 3)
;; (listdiff)
;; (define ld (listdiff 1 2 3))
;; (car-ld ld)
;; (cdr-ld ld)
;; (car-ld (cdr-ld ld))
;; (cdr-ld (cdr-ld ld))
;; (car-ld (cdr-ld (cdr-ld ld)))
;; (cdr-ld (cdr-ld (cdr-ld ld)))


;; (define x '(i o u . y))
;; (listdiff? (cons x (cdr (cdr x))))
;; (cons-ld 'e (cons x (cdr (cdr x))) ) ; '((e i o u . y) '(u . y))

;; (length-ld d1)
;; (length-ld d2)
;; (length-ld d3)
;; (length-ld d6)
;; (length-ld d7)
;; (display d1)
;; (display "\n")
;; (display d2)
;; (display "\n")
;; (display d6)
;; (display "\n")
;; (display d7)
;; (display "\n")
;; (car-ld d7)
;; (car-ld (cdr-ld d7))
;; (car-ld (cdr-ld(cdr-ld d7)))
;; (define x1 '(a b . c))
;; (define ld1 (cons x1 (cdr x1)))
;; (define x2 '(d e . f))
;; (define ld2 (cons x2 (cdr(cdr x2))))
;; (define x3 '(g h . i))
;; (define ld3 (cons x3 (cdr(cdr x3))))
;; (define ld4 (cons '() '()))
;; (append-ld ld1 ld2 ld4)
;; (define kv1 (cons d1 'a))
;; (define kv2 (cons d2 'b))
;; (define kv3 (cons d3 'c))
;; (define kv4 (cons d1 'd))
;; (define d8 (listdiff kv1 kv2 kv3 kv4))
;; (define d9 (listdiff kv3 kv4))

;; (eq? d8 (list-tail-ld d8 0))
;; (equal? (listdiff->list (list-tail-ld d8 2)) (listdiff->list d9))
;; (null-ld? (list-tail-ld d8 4))
;; (list-tail-ld d8 -1)
;; (list-tail-ld d8 5)

;; (eq? (car-ld d6) ils)
;; (eq? (car-ld (cdr-ld d6)) d1)
;; (eqv? (car-ld (cdr-ld (cdr-ld d6))) 37)
;; (equal? (listdiff->list d6) (list ils d1 37))
;; (eq? (list-tail (car d6) 3) (cdr d6))
;; (define ns (make-base-namespace))
;; (listdiff->list (eval e1 ns))
;; (equal? (listdiff->list (eval e1 ns)) (listdiff->list d1))


