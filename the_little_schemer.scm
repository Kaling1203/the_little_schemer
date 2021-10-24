(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? a (car lat))
	       (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (cdr lat))
     (else (cons (car lat)
		 (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (cond
		  ((atom? (car l)) (car l))
		  (else (car (car l))))
		 (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons (car lat)
				(cons new
				      (cdr lat))))
     (else (cons (car lat)
		 (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new lat))
     (else (cons (car lat)
		 (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new
				(cdr lat)))
     (else (cons (car lat)
		 (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? o1 (car lat))
	  (eq? o2 (car lat)))
      (cons new (cdr lat)))
     (else (cons (car lat)
		 (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat))
      (multirember a (cdr lat)))
     (else (cons
	    (car lat)
	    (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat))
      (cons old
	    (cons new
		  (multiinsertR new old (cdr lat)))))
     (else (cons (car lat)
		 (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((cond
       ((null? lat) '())
       ((eq? old (car lat))
	(cons new
	      (cons old
		    (multiinsertL new old (cdr lat)))))
       (else (cons (car lat)
		   (multiinsertL new old (cdr lat)))))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define tup?
  (lambda (tup)
    (cond
     ((null? tup) #t)
     ((number? (car tup)) (tup? (cdr tup)))
     (else #f))))

(define addup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (+ (car tup)
	      (addup (cdr tup)))))))

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (+ n (x n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (+ (car tup1) (car tup2))
		 (tup+ (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
     ((> n m) #f)
     ((< n m) #f)
     (else #t))))

(define up-arrow
  (lambda (n m)
    (cond
     ((eq? m 1) n)
     (else (* n (up-arrow n (sub1 m)))))))

(define pick
  (lambda (n lat)
    (cond
     ((eq? n 1) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat)
		 (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat)
		 (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat))
      (cons (car lat)
	    (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? a (car lat))
      (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (= n 1)))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? a (car l)) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
     (else
      (cons (rember* a (car l))
	    (rember* a (cdr l)))))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? old (car l))
	(cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
     (else
      (cons (insertR* new old (car l))
	    (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? a (car l)) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else (+ (occur* a (car l))
	      (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? old (car l))
	(cons new (subst* new old (cdr l))))
       (else
	(cons (car l) (subst* new old (cdr l))))))
     (else
      (cons (subst* new old (car l))
	    (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? old (car l))
	(cons new (cons old (insertL* new old (cdr l)))))
       (else
	(cons (car l) (insertL* new old (cdr l))))))
     (else
      (cons (insertL* new old (car l))
	    (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (cond
       ((eq? a (car l)) #t)
       (else (member* a (cdr l)))))
     (else
      (or (member* a (car l))
	  (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (cond
       ((eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
       (else #f)))
     ((or (atom? (car l1)) (atom? (car l2))) #f)
     (else
      (and (eqlist? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (l1 l2)
    (cond
     ((and (atom? l1) (atom? l2))
      (eq? l1 l2))
     ((or (atom? l1) (atom? l2)) #f)
     (else (eqlist? l1 l2)))))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((or (eq? '+ (car (cdr aexp)))
	  (eq? 'x (car (cdr aexp)))
	  (eq? 'up-arrow (car (cdr aexp))))
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp))))))
     (else #f))))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
     (else (cons (car lat) (makeset (cdr lat)))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2) (subset? (cdr set1) set2))
     (else #f))))

(define eqset?
  (lambda (set1 set2)
    (and
     (subset? set1 set2)
     (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (or (member? (car set1) set2)
	       (intersect? (cdr set1) set2))
	   #t))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (union (cdr set1) set2))
     (else (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
		      (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel))
		 (revrel (cdr rel)))))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
