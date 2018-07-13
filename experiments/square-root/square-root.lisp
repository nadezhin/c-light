(in-package "ACL2")
;(include-book "std/util/defrule" :dir :system)
;(include-book "centaur/fty/top" :dir :system)

(include-book "ceil-root")
(local (acl2::allow-arith5-help))

; Constraints on inf_T, sup_T, delta_T

(encapsulate
  (((inf_T) => *)
   ((sup_T) => *)
   ((delta_T) => *))

  (local (defun inf_T () #f2.001))
  (local (defun sup_T () #f3.000))
  (local (defun delta_T () 1/3))

  (defruled inf_T-constraint
    (and (real/rationalp (inf_T))
         (< 2 (inf_T))))

  (defruled sup_T-constraint
    (and (real/rationalp (sup_T))
         (< 2 (sup_T))
         (integerp (/ (sup_T) (delta_T)))))

  (defruled delta_T-constraint
    (and (acl2-numberp (delta_T))
         (< 0 (delta_T))
         (< (delta_T) 1/2)
         (integerp (/ (delta_T))))))

; Trivial corollaries form constraints

(defrule inf_T-type
  (and (real/rationalp (inf_T))
       (< 0 (inf_T))
       (not (= (inf_T) 1)))
  :rule-classes :type-prescription)

(defrule inf_T-linear
  (< 2 (inf_T))
  :rule-classes :linear)

(defrule sup_T-type
  (and (real/rationalp (sup_T))
       (< 0 (sup_T))
       (not (= (sup_T) 1)))
  :rule-classes :type-prescription)

(defrule sup_T-linear
  (< 2 (sup_T))
  :rule-classes :linear)

(defrule delta_T-type
  (and (rationalp (delta_T))
       (not (integerp (delta_T)))
       (< 0 (delta_T)))
  :rule-classes :type-prescription
  :use delta_T-constraint)

(with-arith5-help
 (defrule delta_T-linear
   (<= (delta_T) 1/3)
   :rule-classes :linear
   :use delta_T-constraint
   :cases ((<= (/ (delta_T)) 2) (<= 3 (/ (delta_T))))))

(with-arith5-help
 (defrule /-delta_T-type
   (and (integerp (/ (delta_T)))
        (< 1 (/ (delta_T))))
   :rule-classes :type-prescription
   :use delta_T-constraint))

(with-arith5-help
 (defrule /-delta_T-linear
   (<= 3 (/ (delta_T)))
   :rule-classes :linear))

(define /delta_T ()
  :returns (/delta_T (and (integerp /delta_T) (< 1 /delta_T))
                     :rule-classes :type-prescription)
  (/ (delta_T))
  ///
  (defrule /delta_T-linear
    (<= 3 (/delta_T))
    :rule-classes :linear))

; Defintion and properties of Val_T

(define Val_T-p (x)
  (and (acl2-numberp x)
       (integerp (/ x (delta_T)))
       (<= (- (inf_T)) x)
       (<= x (sup_T)))
  :returns (yes booleanp :rule-classes ())
  ///
  (in-theory (disable (Val_T-p)))
  (defrule rational-when-Val_T
    (implies (Val_T-p x)
             (rationalp x))
    :rule-classes :compound-recognizer)
  (defrule Val_T-fwd
    (implies (Val_T-p x)
             (and (<= (- (inf_T)) x)
                  (<= x (sup_T))))
    :rule-classes :forward-chaining)
  (defrule in-grid-Val_T
    (implies (Val_T-p x)
             (integerp (* (/ (delta_T)) x))))
  (with-arith5-nonlinear-help
   (defruled Val_T-distance
     (implies (and (Val_T-p x)
                   (Val_T-p y)
                   (not (= x y)))
              (<= (delta_T) (abs (- x y))))
     :rule-classes :linear
     :use (:instance lemma
                     (x (/ x (delta_T)))
                     (y (/ y (delta_T))))
     :prep-lemmas
     ((defruled lemma
        (implies (and (integerp x)
                      (integerp y)
                      (not (= x y)))
                 (<= 1 (abs (- x y)))))))))
#|
  (defruled Val_T-suff
    (implies (and (acl2-numberp x)
                  (integerp (/ x (delta_T)))
                  (<= (- (inf_T)) x)
                  (<= x (sup_T)))
             (Val_T-p x))))
|#

(defruled Val_T-when-integer
  (implies (integerp x)
           (equal (Val_T-p x)
                  (and (<= (- (inf_T)) x)
                       (<= x (sup_T)))))
  :enable Val_T-p)

(defrule Val_T-0
  (Val_T-p 0)
  :enable Val_T-when-integer)

(defrule Val_T-delta_T
  (Val_T-p (delta_T))
  :enable Val_T-p)

(defrule Val_T-sup_T
  (Val_T-p (sup_T))
  :enable (Val_T-p sup_T-constraint))

(define Val_T-fix
  ((x Val_T-p))
  :returns (fix Val_T-p)
  (if (Val_T-p x) x 0)
  ///
  (fty::deffixtype Val_T
    :pred Val_T-p
    :fix Val_T-fix
    :equiv Val_T-equiv
    :define t))

(define Val_T-encode
  ((x Val_T-p))
  :returns (n integerp :rule-classes :type-prescription)
  (/ (Val_T-fix x) (delta_T))
  ///
  (fty::deffixequiv Val_T-encode))

(define Val_T-decode
  ((n integerp))
  :returns (x rationalp :rule-classes :type-prescription)
  (* (lifix n) (delta_T))
  ///
  (fty::deffixequiv Val_T-decode))

(acl2::with-arith5-nonlinear-help
 (define inf_I ()
   (floor (inf_T) (delta_T))
   :returns (inf_I (and (integerp inf_I) (< 1 inf_I))
                   :rule-classes :type-prescription)
   ///
   (in-theory (disable (inf_I)))
   (defruled inf_I-linear
     (and (<= 4 (inf_I))
          (<= (inf_I) (/ (inf_T) (delta_T))))
     :rule-classes ((:linear :trigger-terms ((inf_I)))))))

(acl2::with-arith5-nonlinear-help
 (define sup_I ()
   (floor (sup_T) (delta_T))
   :returns (sup_I (and (integerp sup_I) (< 1 sup_I))
                   :rule-classes :type-prescription)
   ///
   (in-theory (disable (sup_I)))
   (defrule sup_I-linear
     (and (<= 4 (sup_I))
          (<= (sup_I) (/ (sup_I) (delta_T))))
     :rule-classes ((:linear :trigger-terms ((sup_I)))))))

; Example of rounding. Used in operations constraints witnesses

(define truncate_T
  ((x real/rationalp))
  :returns (result rationalp)
  (* (delta_T) (truncate x (delta_T)))
  ///
  (fty::deffixequiv truncate_T)
  (with-arith5-help
   (defrule Val_T-truncate_T
     (implies (and (<= (- (inf_T)) x)
                   (<= x (sup_T)))
              (Val_T-p (truncate_T x)))
     :enable Val_T-p))
  (with-arith5-help
   (defrule truncate_T-error
     (implies (real/rationalp x)
              (< (abs (+ (- x) (truncate_T x))) (delta_T)))
     :rule-classes :linear)))

; Operation constraints and witnesses

(encapsulate
  (((+_T * *) => *)
   ((-_T * *) => *)
   ((*_T * *) => *)
   ((/_T * *) => *))

  (local (defun +_T (x y) (+ x y)))

  (local (defun -_T (x y) (- x y)))

  (local (defun *_T (x y) (truncate_T (* x y))))

  (local (defun /_T (x y) (truncate_T (/ x y))))

  (defrule +_T-constraint
    (implies (and (Val_T-p x)
                  (Val_T-p y)
                  (<= (- (inf_T)) (+ x y))
                  (<= (+ x y) (sup_T)))
             (equal (+_T x y)
                    (+ x y))))

  (defrule -_T-constraint
    (implies (and (Val_T-p x)
                  (Val_T-p y)
                  (<= (- (inf_T)) (+ x y))
                  (<= (+ x y) (sup_T)))
             (equal (+_T x y)
                    (+ x y))))

  (defrule *_T-constraint
    (implies (and (Val_T-p x)
                  (Val_T-p y)
                  (<= (- (inf_T)) (* x y))
                  (<= (* x y) (sup_T)))
             (and (Val_T-p (*_T x y))
                  (< (abs (- (*_T x y) (* x y))) (delta_T))))
    :disable abs)

  (defrule /_T-constraint
    (implies (and (Val_T-p x)
                  (Val_T-p y)
                  (<= (- (inf_T)) (/ x y))
                  (<= (/ x y) (sup_T)))
             (and (Val_T-p (/_T x y))
                  (< (abs (- (/_T x y) (/ x y))) (delta_T))))
    :disable abs))

; Theorems about constrint operations

(defrule *_T-exact
  (implies (and (Val_T-p x)
                (Val_T-p y)
                (Val_T-p (* x y)))
           (equal (*_T x y)
                  (* x y)))
  :disable abs
  :use (:instance Val_T-distance
                  (x (*_T x y))
                  (y (* x y))))

(defrule /_T-exact
  (implies (and (Val_T-p x)
                (Val_T-p y)
                (Val_T-p (/ x y)))
           (equal (/_T x y)
                  (/ x y)))
  :disable abs
  :use (:instance Val_T-distance
                  (x (/_T x y))
                  (y (/ x y))))

(encapsulate
  (((eps) => *)
   ((stp) => *))

  (local (defun eps () (delta_T)))
  (local (defun stp () (delta_T)))

  (defruled eps-constraint
    (and (Val_T-p (eps))
         (<= (delta_T) (eps))))

  (defruled stp-constraint
    (and (Val_T-p (stp))
         (<= (delta_T) (stp))
         (integerp (/ (stp) (eps)))
         (integerp (/ (sup_T) (stp))))
:enable sup_T-constraint))

; Trivial corollaries form constraints

(defrule eps-type
  (and (Val_T-p (eps))
       (<= (delta_T) (eps)))
  :enable eps-constraint
  :rule-classes :type-prescription)

(defrule eps-linear
       (and (<= (delta_T) (eps))
            (< 0 (eps)))
  :use (delta_T-constraint eps-constraint)
  :rule-classes :linear)

(defrule stp-type
  (and (Val_T-p (stp))
       (<= (delta_T) (stp))
       (integerp (/ (stp) (eps)))
       (integerp (/ (sup_T) (stp))))
  :enable stp-constraint
  :rule-classes :type-prescription)

(defrule stp-linear
  (and (<= (delta_T) (stp))
       (< 0 (stp)))
  :use (delta_T-constraint stp-constraint)
  :rule-classes :linear)

; Defintion and properties of Arg_Stp

(define Arg_Stp-p (x)
  (and (Val_T-p x)
       (integerp (/ x (stp)))
       (< 1 x))
  :returns (yes booleanp :rule-classes ())
  ///
  (in-theory (disable (Arg_Stp-p)))
  (defrule Val_T-when-Arg_Stp
    (implies (Arg_Stp-p x)
             (Val_T-p x))
    :rule-classes :compound-recognizer)
  (defrule rational-when-Arg_Stp
    (implies (Arg_Stp-p x)
             (rationalp x))
    :rule-classes :compound-recognizer)
  (defrule Arg_Stp-fwd
    (implies (Arg_Stp-p x)
             (and (<= (- (inf_T)) x)
                  (<= x (sup_T))))
    :rule-classes :forward-chaining)
  (defrule in-grid-Arg_Stp
    (implies (Arg_Stp-p x)
             (integerp (* (/ (stp)) x)))))

(defruled root_delta_T-lemma
  (implies (<= x (sup_T))
           (<= (* (delta_T) (ceil-root (* (expt (delta_T) -2) x)))
               (sup_T)))
  :cases
  ((not (> (* (expt (delta_T) -2) x)
           (expt (1- (ceil-root (* (expt (delta_T) -2) x))) 2)))
   (not (> (expt (1- (ceil-root (* (expt (delta_T) -2) x))) 2)
           (expt (1- (/ (sup_T) (delta_T))) 2)))
   (not (> (expt (1- (/ (sup_T) (delta_T))) 2)
           (* (sup_T) (- (sup_T) (* 2 (delta_T))) (expt (delta_T) -2))))
   (not (> (* (sup_T) (- (sup_T) (* 2 (delta_T))) (expt (delta_T) -2))
           (* (sup_T) (expt (delta_T) -2))))
   (not (>= (* (sup_T) (expt (delta_T) -2))
            (* x (expt (delta_T) -2)))))
  :hints
  (("subgoal 5" :use (:instance ceil-root-lower-bound
                                (x (* (expt (delta_T) -2) x))))
   ("subgoal 4" :use lemma4)
   ("subgoal 3" :use lemma3)
   ("subgoal 2" :use lemma2)
   ("subgoal 1" :use lemma1))
  :prep-lemmas
  ((with-arith5-nonlinear-help
    (defruled lemma0
      (implies (and (real/rationalp x)
                    (real/rationalp y)
                    (<= 0 x)
                    (< x y))
               (< (expt x 2) (expt y 2)))))
   (with-arith5-nonlinear-help
    (defruled lemma4
      (implies (<= (expt (1- (ceil-root (* (expt (delta_T) -2) x))) 2)
                   (expt (1- (/ (sup_T) (delta_T))) 2))
               (<= (* (delta_T) (ceil-root (* (expt (delta_T) -2) x)))
                   (sup_T)))
      :use (:instance lemma0
                      (x (1- (/ (sup_T) (delta_T))))
                      (y (1- (ceil-root (* (expt (delta_T) -2) x)))))))
   (with-arith5-help
    (defruled lemma3
      (> (expt (1- (/ (sup_T) (delta_T))) 2)
         (* (sup_T) (- (sup_T) (* 2 (delta_T))) (expt (delta_T) -2)))))
   (with-arith5-nonlinear-help
    (defruled lemma2
      (> (* (sup_T) (- (sup_T) (* 2 (delta_T))) (expt (delta_T) -2))
         (* (sup_T) (expt (delta_T) -2)))))
   (with-arith5-help
    (defruled lemma1
      (implies (<= x (sup_T))
               (>= (* (sup_T) (expt (delta_T) -2))
                   (* x (expt (delta_T) -2))))))))

(with-arith5-help
 (define root_delta_T
   ((x Arg_Stp-p))
   :returns (result Val_T-p
                    :hints (("goal" :in-theory (enable Val_T-p Arg_Stp-p
                                                       root_delta_T-lemma))))
   (if (Arg_Stp-p x)
       (* (delta_T) (ceil-root (* x (expt (delta_T) -2))))
     1)
   ///
   (with-arith5-nonlinear-help
    (defrule root_delta_T-lower-bound
     (b* ((root1 (- (root_delta_T x) (delta_T))))
       (implies (Arg_Stp-p x)
                (< (expt root1 2) x)))
     :enable Arg_Stp-p
     :use (:instance ceil-root-lower-bound
                     (x (* x (expt (delta_T) -2))))))
   (with-arith5-nonlinear-help
    (defrule root_delta_T-upper-bound
      (b* ((root (root_delta_T x)))
        (implies (Arg_Stp-p x)
                 (<= x (expt root 2))))
      :use (:instance ceil-root-upper-bound
                      (x (* x (expt (delta_T) -2))))))))

#|
(define root_delta_T
    ((y Val_T-p)
     (x Arg_Stp-p))
    :returns (result Val_T-p)
    (if
        (<= x 1)
        1
        (if
            (>= (- (* y y) x) 0)
            y
            (root_delta_T (+ y (delta_T)) x)
        )
    )
)
|#

; Example of rounding. Used in operations constraints witnesses

(define ceiling_Arg_Stp
  ((x Val_T-p))
  :returns (result rationalp)
  (* (stp) (ceiling x (stp)))
  ///
  (fty::deffixequiv ceiling_Arg_Stp)
  (with-arith5-help
   (defrule Arg_Stp-ceiling_Arg_Stp
     (implies (and (< 1 x)
                   (Val_T-p x))
              (Arg_Stp-p (ceiling_Arg_Stp x)))
     :enable Arg_Stp-p))
  (with-arith5-help
   (defrule ceiling_Arg_Stp-error
     (implies (real/rationalp x)
              (< (abs (+ (- x) (ceiling_Arg_Stp-error x))) (stp)))
     :rule-classes :linear)))

; Operation constraints and witnesses



(encapsulate
  (((round *) => *)
   ((root *) => *))

  (local (defun round (x) (ceiling_Arg_Stp x)))
  (local (defun root (x) (root_delta_T 1 x)))

  (defrule round-constraint
    (implies (and (Val_T-p u)
                  (< 1 u))
             (and (Arg_Stp-p (round u))
                  (<= u (round u))
                  (< (- (round u) (stp)) u))))

  (defrule root-constraint
    (implies (Arg_Stp-p v)
             (and (Val_T-p (root v))
                  (>= (* (root v) (root v)) v)
                  (< (- (* (root v) (root v)) delta_T) v))))
)
