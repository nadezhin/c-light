(in-package "ACL2")
(include-book "std/util/defrule" :dir :system)
(include-book "centaur/fty/top" :dir :system)

(include-book "tools/with-arith5-help" :dir :system)
(local (allow-arith5-help))

(with-arith5-nonlinear++-help
       (defrule ceiling-monotonicity
         (implies (and (rationalp u)
                       (rationalp v)
                       (rationalp s)
                       (< 0 s)
                       (<= u v))
                  (<= (ceiling u s) (ceiling v s)))))

(with-arith5-nonlinear++-help
       (defrule ceiling-lemma-1
         (implies (and (rationalp u)
                       (< 0 u))
                  (= (ceiling (+ 1 u) 1) (+ 1 (ceiling u 1))))))

(with-arith5-nonlinear++-help
       (defrule ceiling-lemma-1-1
         (implies (rationalp u)
                  (= (ceiling (+ 1 u) 1) (+ 1 (ceiling u 1))))))

(with-arith5-nonlinear++-help
       (defrule ceiling-lemma-2
         (implies (and (rationalp u)
                       (< 0 u))
                  (= (ceiling u 1) (+ 1 (ceiling (- u 1) 1))))))

(with-arith5-nonlinear++-help
       (defrule ceiling-lemma-3
         (implies (rationalp u)
                  (= (ceiling u 1) (+ 1 (ceiling (- u 1) 1))))))

(with-arith5-nonlinear++-help
       (defrule ceiling-lemma-5
         (implies (and (acl2-numberp a) (acl2-numberp b))
         (implies (= b (+ 1 a))
                  (= a (- b 1))))
:rule-classes nil))

(defrule ceiling-lemma-6
         (implies (= (ceiling (+ 1 u) 1) (+ 1 (ceiling u 1)))
                  (= (ceiling u 1) (- (ceiling (+ u 1) 1) 1)))
:use (:instance ceiling-lemma-5
                     (a (ceiling u 1))
                     (b (ceiling (+ 1 u) 1))))

(with-arith5-nonlinear++-help
       (defrule ceiling-lemma-7
         (implies (rationalp u)
                  (= (ceiling u 1) (- (ceiling (+ u 1) 1) 1)))
:use (ceiling-lemma-1-1 ceiling-lemma-6)))

(with-arith5-nonlinear++-help
       (defrule nfix-lemma-1
         (implies (and (integerp a)
                       (<= 0 a))
                  (= (nfix a) a))))

(defrule ceiling-lemma-9
         (implies (and (rationalp u)
                       (< 0 u))
                  (<= 1 (ceiling u 1)))))

(defrule ceiling-lemma-11
         (implies (rationalp u)
                  (= (ceiling (- u 1) 1) (- (ceiling u 1) 1)))
:use ((:instance ceiling-lemma-7 (u (- u 1)))))

(defrule ceiling-lemma-12
         (implies (and (rationalp u)
                       (< 0 u))
                  (<= 0 (ceiling (- u 1) 1)))
:use (ceiling-lemma-11 ceiling-lemma-9))

(defrule ceiling-lemma-15
         (implies (and (rationalp u)
                       (< 0 u))
                  (<= 1 (+ 1 (ceiling (- u 1) 1))))
:use ceiling-lemma-12)

(defrule ceiling-lemma-16
         (implies (and (rationalp u)
                       (< 0 u))
                  (= (ceiling u 1) (nfix (ceiling u 1))))
:use (ceiling-lemma-9 nfix-lemma-1))

(defrule ceiling-lemma-17
         (implies (and (rationalp u)
                       (< 0 u))
                  (= (ceiling (- u 1) 1) (nfix (ceiling (- u 1) 1))))
:use (ceiling-lemma-12 nfix-lemma-1))

(defrule ceiling-lemma-19
         (implies (and (rationalp u)
                       (< 0 u))
                  (= (+ 1 (ceiling (- u 1) 1)) (nfix (+ 1 (ceiling (- u 1) 1)))))
:use (ceiling-lemma-15 nfix-lemma-1))

(defrule ceiling-lemma-20
         (implies (and (rationalp u)
                       (< 0 u))
                  (= (nfix (ceiling u 1)) (nfix (+ 1 (ceiling (- u 1) 1)))))
:use (ceiling-lemma-16 ceiling-lemma-19 ceiling-lemma-2))

(with-arith5-nonlinear++-help
       (defrule nfix-lemma-2
         (implies (and (integerp a)
                       (<= 0 a))
                  (= (nfix (+ 1 a)) (+ 1 (nfix a))))))

(defrule ceiling-lemma-21
         (implies (and (rationalp u)
                       (< 0 u))
                  (= (nfix (+ 1 (ceiling (- u 1) 1))) (+ 1 (nfix (ceiling (- u 1) 1)))))
:use (ceiling-lemma-12 (:instance nfix-lemma-2 (a (ceiling (- u 1) 1)))))

(defrule ceiling-lemma-22
         (implies (and (rationalp u)
                       (< 0 u))
                  (= (nfix (ceiling u 1)) (+ 1 (nfix (ceiling (- u 1) 1)))))
:use (ceiling-lemma-20 ceiling-lemma-21))

(defrule ceiling-lemma-23
         (implies (and (rationalp u)
                       (< 0 u))
                  (< (nfix (ceiling (- u 1) 1)) (nfix (ceiling u 1))))
:use (ceiling-lemma-22))

(with-arith5-nonlinear++-help
(defrule lemma-1
     (equal
         (rfix (rfix a)) (rfix a))))

(with-arith5-nonlinear++-help
(defrule lemma-2
     (equal
         (rfix (+ (rfix a) (rfix b))) (+ (rfix a) (rfix b)))))

(with-arith5-nonlinear++-help
(defrule lemma-3
 (implies
  (and
   (rationalp d)
   (< 0 d))
     (equal
         (* (+ (rfix x) (- (rfix y))) (/ (rfix d)))
         (+ (* (+ (rfix (rfix x)) (- (rfix (+ (rfix d) (rfix y)))))
            (/ (rfix (rfix d)))) 1)))))

(with-arith5-nonlinear++-help
(defrule lemma-5
 (implies
  (and
   (rationalp d)
   (< 0 d))
     (equal
         (* (+ (rfix (rfix x)) (- (rfix (+ (rfix d) (rfix y)))))
            (/ (rfix (rfix d))))
         (- (* (+ (rfix x) (- (rfix y))) (/ (rfix d))) 1)))))

(define root-linear-aux
   ((x rationalp)
    (y rationalp)
    (d rationalp))
   :hints (("goal" :use ((:instance lemma-5 (x x) (y y) (d d))
                         (:instance ceiling-lemma-23
   (u  (* (+ (rfix x) (- (rfix y))) (/ (rfix d))))))))
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
     (root-linear-aux x (+ d y) d)))
