#lang racket

;; object structure.  An object contains two, each is a list
;; of mutable cons cell(association list).
;; - vars    : association list of variables name and value (mutable pair)
;; - methods : association list of methods of the object (immutable pair)
;; - father  : the object's facter in inheritance, if there does not
;;             inherit from any other object, this filed is #f
(struct object (vars methods father))


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
        (error "set non exsiting field" field)
        (set-mcdr! ptr new-val))))

;; send : Send message to object with argument
;; - obj : the object
;; - msg : the message to send to the object
;; - args: argument with the message
(define (send obj msg args)
  (let ([ptr (assoc msg (object-methods obj))])
    (if ptr
        ((cdr ptr) obj args)
        (apply (object-father obj) msg args))))


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
                              (sqrt (+ (* x x) (* y y)))))))
              #f)])
    (lambda (method . args)
      (cond [(equal? method 'get-x) (send obj 'get-x args)]
            [(equal? method 'get-y) (send obj 'get-y args)]
            [(equal? method 'set-x) (send obj 'set-x args)]
            [(equal? method 'set-y) (send obj 'set-y args)]
            [(equal? method 'dist-to-origin) (send obj 'dist-to-origin args)]
            [else (error "undefined method for object" method)]))))

(define (new-point-3d [x 0] [y 0] [z 0])
  (let ([obj (object
              (list (mcons 'z z))
              (list (cons 'get-z (lambda (self args) (get self 'z)))
                    (cons 'set-z (lambda (self args) (set self 'z (car args))))
                    (cons 'dist-to-origin
                          (lambda (self args)
                            (let ([x (send self 'get-x args)]
                                  [y (send self 'get-y args)]
                                  [z (send self 'get-z args)])
                              (sqrt (+ (* x x) (* y y) (* z z)))))))
              ;; father object of this object
              (new-point x y))])
    (lambda (method . args)
      (cond [(equal? method 'get-x) (send obj 'get-x args)]
            [(equal? method 'get-y) (send obj 'get-y args)]
            [(equal? method 'get-z) (send obj 'get-z args)]
            [(equal? method 'set-x) (send obj 'set-x args)]
            [(equal? method 'set-y) (send obj 'set-y args)]
            [(equal? method 'set-z) (send obj 'set-z args)]
            [(equal? method 'dist-to-origin) (send obj 'dist-to-origin args)]
            [else (error "unknow method call" method)]))))


;> (define p3 (new-point-3d 2 2 2))
;> (p3 'get-x)
;2
;> (p3 'get-y)
;2
;> (p3 'get-z)
;2
;> (p3 'dist-to-origin)
;3.4641016151377544
;> 