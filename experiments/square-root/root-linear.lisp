(in-package "ACL2")
(include-book "std/util/defrule" :dir :system)
(include-book "centaur/fty/top" :dir :system)

(include-book "tools/with-arith5-help" :dir :system)
(local (allow-arith5-help))

(with-arith5-nonlinear++-help
       (defrule ceiling-lemma-1
         (implies (rationalp u)
                  (= (ceiling (+ 1 u) 1) (+ 1 (ceiling u 1))))))

(with-arith5-nonlinear++-help
       (defrule ceiling-lemma-2
         (implies (and (acl2-numberp a) (acl2-numberp b))
         (implies (= b (+ 1 a))
                  (= a (- b 1))))
:rule-classes nil))

(defrule ceiling-lemma-3
         (implies (= (ceiling (+ 1 u) 1) (+ 1 (ceiling u 1)))
                  (= (ceiling u 1) (- (ceiling (+ u 1) 1) 1)))
:use (:instance ceiling-lemma-2
                     (a (ceiling u 1))
                     (b (ceiling (+ 1 u) 1))))

(with-arith5-nonlinear++-help
       (defrule ceiling-lemma-5
         (implies (rationalp u)
                  (= (ceiling u 1) (- (ceiling (+ u 1) 1) 1)))
:use (ceiling-lemma-1 ceiling-lemma-3)))

(with-arith5-help
 (define root-linear-aux
   ((x rationalp)
    (y rationalp)
    (d rationalp))
   :measure (nfix (ceiling (/ (- (rfix x) (rfix y)) (rfix d)) 1))
   :returns (result rationalp)
   (b* ((x (rfix x))
        (y (rfix y))
        (d (rfix d))
        ((when (<= d 0)) -1)
        ((when (<= x 1)) -1)
        ((when (<= y 1)) -1)
        ((when (<= x y)) -1)
        ((when (<= x (* y y))) y))
     (root-linear-aux x (+ d y) d))))
