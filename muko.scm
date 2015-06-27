;;  muko.scm  jpt4  20130130 
;;  rev. UTC20150609
;;  microKanren in miniKanren

;use tagged lists instead of vectors

(define (varo i o) (== `(var . ,i) o))
(define (var?o i) (caro i 'var))
;;  the equivalent of # for vectors is (var . _) for tagged lists
;;  (vector-ref n 0) accesses the first element of the vector
(define (var=?o i1 i2) (fresh (a)
                         (== `(var . ,a) i1)
                         (== `(var . ,a) i2)))

(define (walko u s o)
  (conde
    [(fresh (a d res c)
       (var?o u) (asspo (lambda (v) (var=?o u v)) s res)
       (== `((var . ,a) . ,d) res)
       (cdro res c)
       (walko c s o))]
    [(fresh (a)
       (=/= `(var . ,a) u)
       (== u o))]))

(define (ext-so x v s o) (== `((,x . ,v) . ,s) o))

#;(define (==o u v o)
  (lambda (s/c)))

(define (asspo proc ls o)
  (conde
    [(fresh (aa ad d)
       (== `((,aa . ,ad) . ,d) ls)
       (proc aa)
       (== `(,aa . ,ad) o))]
    [(fresh (d)
       (cdro ls d)
       (asspo proc d o))]))

(define (even?o i)
  (fresh (d)
    (== `(0 . ,d) i)
    (=/= '() d)))

(define (cdro i o)
  (fresh (a)
    (== `(,a . ,o) i)))

(define (not-var?o i o)
  (conde
    [(var?o i) (== o 'fail)]))

;translate let to lambda
#;(define (walk u s)
  ((lambda (pr)
     (if pr (walk (cdr pr) s) u))
   (and (var? u) (assp (lambda (v) (var=? u v)) s))))

(define (test a b o)
  ((lambda (c)
     (conde
       [c (conso a b o)]))
   (conde
     [(=/= a b) (=/= 'a a)])))
  
(define s '(((var . 1) . 1)))
(define u '(var . 1))
