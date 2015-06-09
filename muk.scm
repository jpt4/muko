;;  muko.scm
;;  microKanren in miniKanren
;;  jpt4
;;  20130130
;;  revised UTC20150609

;use tagged lists instead of vectors

(define (var-o i o) (== `(var . ,i) o))
(define (var?-o i) (caro i 'var))
;the equivalent of # for vectors is (var . _) for tagged lists
;(vector-ref n 0) accesses the first element of the vector
(define (var=?-o i1 i2) (fresh (a)
                          (== `(var . ,a) i1)
                          (== `(var . ,a) i2)))

(define (walk-o u s o)
  (conde
    [(fresh (a d res c)
       (var?-o u) (assp-o (lambda (v) (var=?-o u v)) s res)
       (== `((var . ,a) . ,d) res)
       (cdro res c)
       (walk-o c s o))]
    [(fresh (a)
       (=/= `(var . ,a) u)
       (== u o))]))

(define (assp-o proc ls o)
  (conde
    [(fresh (aa ad d)
       (== `((,aa . ,ad) . ,d) ls)
       (proc aa)
       (== `(,aa . ,ad) o))]
    [(fresh (d)
       (cdro ls d)
       (assp-o proc d o))]))

(define (not-var?-o i o)
  (conde
    [(var?-o i) (== o 'fail)]))
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
