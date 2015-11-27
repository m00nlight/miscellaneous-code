#lang racket

;; object structure.  An object contains two, each is a list
;; of mutable cons cell(association list).
;; - vars    : association list of variables name and value (mutable pair)
;; - methods : association list of methods of the object (immutable pair)
(struct object (vars methods))


;; assoc-m : get corespond value of an item in an association list
;; type : Symbol * Listof[(Symbol, SchemeVal)] -> (Symbol, SchemeVal)
(define (assoc-m name xs)
  (cond [(null? xs) #f]
        [(equal? name (mcar (car xs))) (car xs)]
        [else (assoc-m name (cdr xs))]))

(define (get obj field)
  (let ([ptr (assoc-m field (object-vars obj))])
    (if (false? ptr)
        (error "access non exsiting field:" field)
        (mcdr ptr))))

(define (set obj field new-val)
  (let ([ptr (assoc-m field (object-vars obj))])
    (if (false? ptr)
        (error "set non exsiting field:" field)
        (set-mcdr! ptr new-val))))

;; send : Send message to object with argument
;; - obj : the object
;; - msg : the message to send to the object
;; - args: argument with the message
(define (send obj msg . args)
  (let ([ptr (assoc msg (object-methods obj))])
    (if (false? ptr)
        (error "method not found:" msg)
        ((cdr ptr) obj args))))

;; make one point object
(define p1
  (object (list (mcons 'x 0)
                (mcons 'y 0))
          (list (cons 'get-x (lambda (self args) (get self 'x)))
                (cons 'get-y (lambda (self args) (get self 'y)))
                (cons 'set-x (lambda (self args) (set self 'x (car args))))
                (cons 'set-y (lambda (self args) (set self 'y (car args))))
                (cons 'dist-to-origin
                      (lambda (self args)
                        (let ([x (send self 'get-x args)]
                              [y (send self 'get-y args)])
                          (sqrt (+ (* x x) (* y y)))))))))
  

;> (send p1 'get-x)
;0
;> (send p1 'set-x 3)
;> (send p1 'set-y 4)
;> (send p1 'dist-to-origin)
;5
;> 
  