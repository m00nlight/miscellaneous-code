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
        (error "access non exsiting field" field)
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
(define (send obj msg args)
  (let ([ptr (assoc msg (object-methods obj))])
    (if (false? ptr)
        (error "method not found:" msg)
        ((cdr ptr) obj args))))


(define (new-point [x 0] [y 0])
  (let ([obj (object
              (list (mcons 'x x)
                    (mcons 'y y))
              (list (cons 'get-x (lambda (self args) (get self 'x)))
                    (cons 'get-y (lambda (self args) (get self 'y)))
                    (cons 'set-x (lambda (self args) (set self 'x (car args))))
                    (cons 'set-y (lambda (self args) (set self 'y (car args))))
                    (cons 'dist-to-origin
                          (lambda (self args)
                            (let ([x (send self 'get-x args)]
                                  [y (send self 'get-y args)])
                              (sqrt (+ (* x x) (* y y))))))))])
    (lambda (method . args)
      (cond [(equal? method 'get-x) (send obj 'get-x args)]
            [(equal? method 'get-y) (send obj 'get-y args)]
            [(equal? method 'set-x) (send obj 'set-x args)]
            [(equal? method 'set-y) (send obj 'set-y args)]
            [(equal? method 'dist-to-origin) (send obj 'dist-to-origin args)]
            [else (error "undefined method for object:" method)]))))

;; some test for point class

;> (define pt1 (new-point 3 4))
;> (define pt2 (new-point 5 12))
;> (pt1 'dist-to-origin)
;5
;> (pt2 'dist-to-origin)
;13
;> 


(define (new-circle [x 0] [y 0] [r 0])
  (let ([obj (object
              (list (mcons 'x x)
                    (mcons 'y y)
                    (mcons 'r r))
              (list
               (cons 'get-x (lambda (self args) (get self 'x)))
               (cons 'get-y (lambda (self args) (get self 'y)))
               (cons 'get-r (lambda (self args) (get self 'r)))
               (cons 'set-x (lambda (self args) (set self 'x (car args))))
               (cons 'set-y (lambda (self args) (set self 'y (car args))))
               (cons 'set-r (lambda (self args) (set self 'r (car args))))
               (cons 'perimeter (lambda (self args)
                                  (* 2 3.14 (send self 'get-r args))))
               (cons 'area (lambda (self args)
                             (let ([r (send self 'get-r args)])
                               (* 3.14 r r))))))])
    (lambda (method . args)
      (cond [(equal? method 'get-x) (send obj 'get-x args)]
            [(equal? method 'get-y) (send obj 'get-y args)]
            [(equal? method 'get-r) (send obj 'get-r args)]
            [(equal? method 'set-x) (send obj 'set-x args)]
            [(equal? method 'set-y) (send obj 'set-y args)]
            [(equal? method 'set-r) (send obj 'set-r args)]
            [(equal? method 'perimeter) (send obj 'perimeter args)]
            [(equal? method 'area) (send obj 'area args)]
            [else (error "undefined method for object:" method)]))))

;; Some test for circle class

;> (define circle (new-circle 2 3 2))
;> (circle 'perimeter)
;12.56
;> (circle 'area)
;12.56
;> 
               
  