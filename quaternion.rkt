(require racket/generic
         (prefix-in racket: (only-in racket conjugate magnitude angle exp log expt sqr sqrt zero? * + - / =)))
(provide (struct-out quaternion) quaternion=? make-quaternion conjugate magnitude angle exp log expt sqr sqrt zero? * + - / =)
(struct quaternion (r i j k)
  #:transparent #:mutable
  #:methods gen:dict
  [(define/generic dref dict-ref)
   (define/generic dset dict-set)
   (define/generic dset! dict-set!)
   (define (dict-ref q k [default (lambda () (error "key not found" k))])
     (case k
       [(0 r) (quaternion-r q)]
       [(1 i) (quaternion-i q)]
       [(2 j) (quaternion-j q)]
       [(3 k) (quaternion-k q)]
       [else (if (procedure? default) (default) default)]))
   (define (dict-set q k v)
     (case k
       [(0 r) (struct-copy quaternion q [r v])]
       [(1 i) (struct-copy quaternion q [i v])]
       [(2 j) (struct-copy quaternion q [j v])]
       [(3 k) (struct-copy quaternion q [k v])]
       [else (error "key not found" k)]))
   (define (dict-remove q k) (dset q k 0))
   (define (dict-set! q k v)
     (case k
       [(0 r) (set-quaternion-r! q v)]
       [(1 i) (set-quaternion-i! q v)]
       [(2 j) (set-quaternion-j! q v)]
       [(3 k) (set-quaternion-k! q v)]
       [else (error "key not found" k)]))
   (define (dict-remove! q k) (dset! q k 0))
   (define (dict-count q) 4)
   (define (dict-iterate-first q) 0)
   (define (dict-iterate-next q pos)
     (and (< pos 3) (add1 pos)))
   (define (dict-iterate-key q pos)
     (list-ref '(r i j k) pos))
   (define (dict-iterate-value q pos)
     (dref q pos))]
  #:methods gen:equal+hash
  [(define (equal-proc a b callback)
     (and (racket:= (quaternion-r a) (quaternion-r b))
          (racket:= (quaternion-i a) (quaternion-i b))
          (racket:= (quaternion-j a) (quaternion-j b))
          (racket:= (quaternion-k a) (quaternion-k b))))
   (define (hash-proc q callback)
     (racket:+ (racket:* 1000000 (quaternion-r q)) (racket:* 10000 (quaternion-i q)) (racket:* 100 (quaternion-j q)) (quaternion-k q)))
   (define (hash2-proc q callback)
     (racket:+ (racket:* 1000000 (quaternion-k q)) (racket:* 10000 (quaternion-j q)) (racket:* 100 (quaternion-i q)) (quaternion-r q)))]
  #:methods gen:custom-write
  [(define (write-proc q port mode)
     (case mode
         [(0 #t) (display `(quaternion ,(quaternion-r q) ,(quaternion-i q) ,(quaternion-j q) ,(quaternion-k q)) port)]
         [(1) (display `#(struct:quaternion ,(quaternion-r q) ,(quaternion-i q) ,(quaternion-j q) ,(quaternion-k q)) port)]
         [else (display (format "~a~a~a~ai~a~aj~a~ak" (if (negative? (quaternion-r q)) "-" "")
                                (abs (quaternion-r q)) (if (negative? (quaternion-i q)) "-" "+")
                                (abs (quaternion-i q)) (if (negative? (quaternion-j q)) "-" "+")
                                (abs (quaternion-j q)) (if (negative? (quaternion-k q)) "-" "+")
                                (abs (quaternion-k q))) port)]))])
(define (make-quaternion r+i j+k)
  (quaternion (real-part r+i) (imag-part r+i) (real-part j+k) (imag-part j+k)))
(define (quaternion=? q x)
  (when (and (not (quaternion? q)) (quaternion? x))
    (let ([tmp q])
      (set! q x) (set! x tmp)))
  (or (equal? q x)
      (cond [(real? x) (equal? q (quaternion x 0 0 0))]
            [(complex? x) (equal? q (quaternion (real-part x) (imag-part x) 0 0))]
            [(symbol? x)
             (case x
               [(+j) (equal? q (quaternion 0 0 1 0))]
               [(+k) (equal? q (quaternion 0 0 0 1))]
               [(-j) (equal? q (quaternion 0 0 -1 0))]
               [(-k) (equal? q (quaternion 0 0 0 -1))]
               [else #f])]
            [else #f])))
(define-generics qn
  (conjugate qn)
  (magnitude qn)
  (angle qn)
  (exp qn)
  (log qn)
  (expt qn n)
  (sqr qn)
  (sqrt qn)
  (zero? qn)
  (+ qn . rem)
  (- qn . rem)
  (* qn . rem)
  (/ qn . rem)
  (= qn . rem)
  #:fast-defaults ([quaternion?
                    (define/generic r* *)
                    (define/generic r/ /)
                    (define/generic r+ +)
                    (define/generic rmag magnitude)
                    (define/generic rexp exp)
                    (define/generic rlog log)
                    (define/generic rsqrt sqrt)
                    (define/generic rz? zero?)
                    (define/generic rang angle)
                    (define/generic r= =)
                    (define (= qn . rem)
                      (or (empty? rem)
                          (and (quaternion=? qn (car rem))
                               (apply r= rem))))
                    (define (zero? q)
                      (and (racket:zero? (quaternion-r q))
                           (racket:zero? (quaternion-i q))
                           (racket:zero? (quaternion-j q))
                           (racket:zero? (quaternion-k q))))
                    (define (conjugate q)
                      (struct-copy quaternion q [i (racket:* -1 (quaternion-i q))] [j (racket:* -1 (quaternion-j q))]
                                   [k (racket:* -1 (quaternion-k q))]))
                    (define (magnitude q)
                      (racket:sqrt (foldl (λ (x sum) (racket:+ sum (racket:sqr x))) 0 (dict-values q))))
                    (define (angle q)
                      (let ([v (struct-copy quaternion q [r 0])]
                            [k (foldl (λ (x sum) (racket:+ sum (racket:sqr x))) 0 (dict-values q))])
                        (if (rz? v) v
                            (r* (r/ v (racket:sqrt (racket:- k (racket:sqr (quaternion-r q)))))
                                (acos (racket:/ (quaternion-r q) (racket:sqrt k)))))))
                    (define (exp q)
                      (let* ([v (struct-copy quaternion q [r 0])]
                             [vm (rmag v)]
                             [e (make-polar (racket:exp (quaternion-r q)) vm)])
                        (r+ (r* (r/ v vm) (imag-part e)) (real-part e))))
                    (define (log q)
                      (r+ (rang q) (racket:log (rmag q))))
                    (define (expt q n)
                      (rexp (r* (rlog q) n)))
                    (define (sqr q)
                      (let ([a (make-rectangular (quaternion-r q) (quaternion-i q))]
                            [b (make-rectangular (quaternion-j q) (quaternion-k q))])
                        (make-quaternion (racket:- (racket:sqr a) (racket:* (racket:conjugate b) b))
                                         (racket:+ (racket:* a b) (racket:* (racket:conjugate a) b)))))
                    (define (sqrt q) (rexp (r/ (rlog q) 2)))
                    (define (+ q . rem)
                      (cond [(empty? rem) q]
                            [(and (quaternion? (car rem)) (car rem))
                             => (λ (x) (apply r+ (apply quaternion (map racket:+ (dict-values q) (dict-values x)))
                                              (cdr rem)))]
                            [(not (real? (car rem)))
                             (apply r+ (struct-copy quaternion q [r (racket:+ (quaternion-r q) (real-part (car rem)))]
                                                    [i (racket:+ (quaternion-i q) (imag-part (car rem)))]) (cdr rem))]
                            [else (apply r+ (struct-copy quaternion q [r (racket:+ (quaternion-r q) (car rem))]) (cdr rem))]))
                    (define (- q . rem)
                      (if (empty? rem) q
                          (r+ q (r* (apply r+ rem) -1))))
                    (define (* q . rem)
                      (cond [(empty? rem) q]
                            [(and (quaternion? (car rem)) (car rem))
                             => (λ (x) (apply r* ((λ (a b c d) (make-quaternion
                                                                (racket:- (racket:* a c)
                                                                          (racket:* (racket:conjugate d) b))
                                                                (racket:+ (racket:* d a)
                                                                          (racket:* b (racket:conjugate c)))))
                                                  (make-rectangular (quaternion-r q) (quaternion-i q))
                                                  (make-rectangular (quaternion-j q) (quaternion-k q))
                                                  (make-rectangular (quaternion-r x) (quaternion-i x))
                                                  (make-rectangular (quaternion-j x) (quaternion-k x))) (cdr rem)))]
                            [(not (real? (car rem)))
                             (apply r* (r+ (r* q (real-part (car rem)))
                                           (r* (quaternion (racket:* -1 (quaternion-i q)) (quaternion-r q)
                                                           (quaternion-k q) (racket:* -1 (quaternion-j q)))
                                               (imag-part (car rem))))
                                    (cdr rem))]
                            [else (apply r* (apply quaternion (map (λ (x) (racket:* (car rem) x)) (dict-values q))) (cdr rem))]))
                    (define (/ q . rem)
                      (apply r* q (map (curry r/ 1) rem)))])
  #:defaults ([complex?
               (define/generic r+ +)
               (define/generic r* *)
               (define/generic r/ /)
               (define/generic rconj conjugate)
               (define/generic r= =)
               (define zero? racket:zero?)
               (define conjugate racket:conjugate)
               (define magnitude racket:magnitude)
               (define angle racket:angle)
               (define exp racket:exp)
               (define log racket:log)
               (define expt racket:expt)
               (define sqrt racket:sqrt)
               (define sqr racket:sqr)
               (define (= x . args)
                 (cond [(empty? args) #t]
                       [(quaternion? (car args))
                        (and (quaternion=? (car args) x)
                             (apply r= x (cdr args)))]
                       [else (and (racket:= x (car args)) (apply r= args))]))
               (define (+ x . args)
                 (cond [(empty? args) x]
                       [(quaternion? (car args))
                        (apply r+ (dict-set (car args) 0 (racket:+ x (quaternion-r (car args)))) (cdr args))]
                       [else (apply r+ (racket:+ x (car args)) (cdr args))]))
               (define (- x . args)
                 (if (empty? args) x
                     (r+ (r* (apply r+ args) -1) x)))
               (define (* x . args)
                 (cond [(empty? args) x]
                       [(quaternion? (car args))
                        (if (not (real? x))
                            (apply r* (r+ (r* (car args) (real-part x))
                                          (r* (quaternion (racket:* -1 (quaternion-i (car args))) (quaternion-r (car args))
                                                          (racket:* -1 (quaternion-k (car args))) (quaternion-j (car args)))
                                              (imag-part x))) (cdr args))
                            (apply r* (apply quaternion (map (curry racket:* x) (dict-values (car args)))) (cdr args)))]
                       [else (apply r* (racket:* x (car args)) (cdr args))]))
               (define (/ x . args)
                 (cond [(empty? args) x]
                       [(quaternion? (car args))
                        (apply r/ (r* x (rconj (car args)))
                               (foldl (λ (x sum) (+ sum (sqr x))) 0 (dict-values (car args)))
                               (cdr args))]
                       [else (apply r/ (racket:/ x (car args)) (cdr args))]))]))
