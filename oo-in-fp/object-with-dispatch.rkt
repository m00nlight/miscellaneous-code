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
        (error "access non exsiting field ~a" field)
        (mcdr ptr))))

(define (set obj field new-val)
  (let ([ptr (assoc-m field (object-vars obj))])
    (if (false? ptr)
        (error "set non exsiting field ~a" field)
        (set-mcdr! ptr new-val))))

;; send : Send message to object with argument
;; - obj : the object
;; - msg : the message to send to the object
;; - args: argument with the message
(define (send obj msg args)
  (let ([ptr (assoc msg (object-methods obj))])
    (if (false? ptr)
        (error "method not found" msg)
        ((cdr ptr) obj args))))

;; make one point object with dispatch
(define ptx
  (let ([obj (object
              (list (mcons 'x 0)
                    (mcons 'y 0))
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
            [else (error "undefined method for object ~a" method)]))))

;> (ptx 'get-x)
;0
;> (ptx 'get-y)
;0
;> (ptx 'set-x 3)
;> (ptx 'set-y 4)
;> (ptx 'dist-to-origin)
;5
;> 
        

  