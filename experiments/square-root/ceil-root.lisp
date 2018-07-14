(in-package "ACL2")
(include-book "std/util/defrule" :dir :system)
(include-book "centaur/fty/top" :dir :system)

(include-book "tools/with-arith5-help" :dir :system)
(local (allow-arith5-help))

(with-arith5-help
 (define ceil-root-aux
   ((x real/rationalp)
    (y natp))
   :measure (nfix (- (ceiling x 1) (nfix y)))
   :returns (result natp)
   (b* ((x (realfix x))
        (y (nfix y))
        ((when (<= x (* y y))) y))
     (ceil-root-aux x (1+ y)))
   ///
   (fty::deffixequiv ceil-root-aux)
   (defrule ceil-root-aux-upper-bound
     (b* ((root (ceil-root-aux x y)))
       (<= (realfix x) (* root root))))
   (defrule ceil-root-aux-lower-bound
     (b* ((root1 (- (ceil-root-aux x y) 1)))
       (implies (and (real/rationalp x)
                     (natp y)
                     (< (* y y) x))
                (and (< (* root1 root1) x)))))))

(define ceil-root
  ((x real/rationalp))
  :returns (result natp)
  (ceil-root-aux x 0)
  ///
  (fty::deffixequiv ceil-root)
  (defrule ceil-root-upper-bound
    (b* ((root (ceil-root x)))
      (<= (realfix x) (* root root)))
    :use (:instance ceil-root-aux-upper-bound (y 0)))
  (defrule ceil-root-lower-bound
    (b* ((root1 (- (ceil-root x) 1)))
      (implies (and (real/rationalp x)
                    (< 0 x))
               (< (* root1 root1) x)))
    :use (:instance ceil-root-aux-lower-bound (y 0)))
  (defruled ceil-root-default
    (implies (or (not (real/rationalp x))
                 (<= x 0))
             (equal (ceil-root x) 0))
    :enable ceil-root-aux))

(local
 (acl2::with-arith5-nonlinear-help
  (defrule sqr-monotone
    (implies (and (real/rationalp x)
                  (real/rationalp y)
                  (<= 0 y)
                  (< (expt x 2) (expt y 2)))
             (< x y)))))

(defruled ceil-root-monotone
  (implies (<= (realfix x) (realfix y))
           (<= (ceil-root x) (ceil-root y)))
  :enable ceil-root-default
  :cases ((< (1- (ceil-root x)) (ceil-root y)))
  :use ((:instance ceil-root-lower-bound
                   (x (realfix x)))
        (:instance sqr-monotone
                   (x (1- (ceil-root x)))
                   (y (ceil-root y)))
        (:instance ceil-root-upper-bound
                   (x y))))

(acl2::with-arith5-help
 (defruled cell-root-unique
   (implies (and (natp n)
                 (or (zp n)
                     (< (expt (- n 1) 2) (realfix x)))
                 (<= (realfix x) (expt n 2)))
            (equal (ceil-root x) n))
   :cases ((< n 1)
           (< (ceil-root x) 1))
   :hints
   (("subgoal 3" :cases ((<= (ceil-root x) (1- n))
                         (>= (ceil-root x) (1+ n))))
    ("subgoal 3.2" :use (ceil-root-upper-bound
                         (:instance sqr-monotone
                                   (x (1- n))
                                   (y (ceil-root x)))))
    ("subgoal 3.1" :use (ceil-root-lower-bound
                         (:instance sqr-monotone
                                   (x (1- (ceil-root x)))
                                   (y n))))
    ("subgoal 1" :cases ((and (real/rationalp x) (< 0 x))))
    ("subgoal 1.2" :use (:instance sqr-monotone
                                   (x (1- n))
                                   (y 0)))
    ("subgoal 1.1" :use ceil-root-upper-bound))))


(with-arith5-help
 (defrule ceil-root-sqr
   (implies (natp x)
            (equal (ceil-root (* x x)) x))
   :use (:instance cell-root-unique
                   (x (* x x))
                   (n x))))
