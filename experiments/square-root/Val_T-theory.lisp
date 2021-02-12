(in-package "ACL2")
(include-book "std/util/defrule" :dir :system)
(include-book "centaur/fty/top" :dir :system)

(include-book "tools/with-arith5-help" :dir :system)
(local (allow-arith5-help))

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

(defrule Val_T-1
  (Val_T-p 1)
  :enable Val_T-when-integer)

(with-arith5-help
(defrule Val_T-1-delta_T
  (Val_T-p (+ 1 (delta_T)))
  :enable (delta_T-constraint
           inf_T-constraint
           sup_T-constraint
           Val_T-1 Val_T-delta_T Val_T-p)))

(defrule integer-1-delta_T
    (integerp (* (/ (delta_T)) (+ 1 (delta_T))))
    :use Val_T-1-delta_T
    :enable Val_T-p)

(defrule Val_T-sup_T
  (Val_T-p (sup_T))
  :enable (Val_T-p sup_T-constraint))

(define Val_T-fix
  ((x Val_T-p))
  :returns (fix Val_T-p)
  (if (Val_T-p x) x 0)
  ///
  (defrule Val_T-fix-when-Val_T-p
    (implies (Val_T-p x)
             (equal (Val_T-fix x) x)))
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
  (and (rationalp (stp))
       (< 0 (stp)))
  :rule-classes :type-prescription
  :use stp-constraint)

(defrule integerp-stp/delta_T
  (integerp (* (/ (delta_T)) (stp)))
  :rule-classes :type-prescription
  :use stp-constraint)

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

(define ceiling-stp
  ((x real/rationalp))
  :returns (result rationalp)
  (* (stp) (ceiling x (stp)))
  ///
  (fty::deffixequiv ceiling-stp)
  (with-arith5-help
   (defrule multiple-ceiling-stp
     (integerp (* (/ (stp)) (ceiling-stp x)))))
  (with-arith5-help
   (defrule ceiling-stp-of-multiple-stp
     (implies (integerp (/ x (stp)))
              (equal (ceiling-stp x) (realfix x)))))
  (acl2::with-arith5-help
   (defrule ceiling-stp-monotone
     (implies (<= (realfix x) (realfix y))
              (<= (ceiling-stp x) (ceiling-stp y)))
     :cases ((and (real/rationalp x) (real/rationalp y)))
     :hints (("subgoal 2" :in-theory (enable ceiling)))
     :disable rewrite-ceiling-to-floor
     :prep-lemmas
     ((with-arith5-nonlinear++-help
       (defrule lemma
         (implies (and (real/rationalp x)
                       (real/rationalp y)
                       (real/rationalp d)
                       (< 0 d)
                       (<= x y))
                  (<= (ceiling x d) (ceiling y d))))))))
  (with-arith5-help
   (defrule ceiling-stp-linear
     (and (<= (realfix x) (ceiling-stp x))
          (< (ceiling-stp x) (+ (realfix x) (stp))))
     :rule-classes ((:linear :trigger-terms ((ceiling-stp x)))))))

(defrule Val_T-ceiling-stp
   (implies (and (<= (- (inf_T)) (realfix x))
                 (<= (realfix x) (sup_T)))
            (Val_T-p (ceiling-stp x)))
   :enable Val_T-p
   :prep-lemmas
   ((with-arith5-help
     (defrule lemma1
       (integerp (* (/ (delta_T)) (ceiling-stp x)))
       :enable intp-*
       :disable multiple-ceiling-stp
       :use (multiple-ceiling-stp
             (:instance intp-1
                        (x (* (/ (STP)) (CEILING-STP X)))
                        (y (* (/ (DELTA_T)) (STP)))))))
    (defrule lemma2
      (implies (<= (realfix x) (sup_T))
               (<= (ceiling-stp x) (sup_T)))
      :enable stp-constraint
      :use ((:instance ceiling-stp-monotone
                       (y (sup_T)))
            (:instance ceiling-stp-of-multiple-stp
                       (x (sup_T)))))))

(defrule ceiling-stp-error
  (implies (real/rationalp x)
           (< (abs (+ (- x) (ceiling-stp x))) (stp)))
  :rule-classes :linear)

(with-arith5-help
 (define ceiling_Arg_Stp
   ((x Val_T-p))
   :returns (result rationalp :rule-classes :type-prescription)
   (ceiling-stp (Val_T-fix x))
   ///
   (fty::deffixequiv ceiling_Arg_Stp)
   (defrule Arg_Stp-ceiling_Arg_Stp
     (implies (and (< 1 x)
                   (Val_T-p x))
              (Arg_Stp-p (ceiling_Arg_Stp x)))
     :enable Arg_Stp-p)
   (defrule ceiling_Arg_Stp-linear
     (and (<= (Val_T-fix x) (ceiling_Arg_Stp x))
          (< (ceiling_Arg_Stp x) (+ (Val_T-fix x) (stp))))
     :rule-classes ((:linear :trigger-terms ((ceiling_Arg_Stp x)))))
   (with-arith5-help
    (defrule ceiling_Arg_Stp-error
      (implies (Val_T-p x)
               (< (abs (+ (- x) (ceiling_Arg_Stp x))) (stp)))
      :rule-classes :linear))))

; Operation constraints and witnesses

(encapsulate
  (((round-stp *) => *))

  (local (defun round-stp (x) (ceiling_Arg_Stp x)))

  (defrule round-stp-constraint
    (implies (and (Val_T-p u)
                  (< 1 u))
             (and (Arg_Stp-p (round-stp u))
                  (<= u (round-stp u))
                  (< (- (round-stp u) (stp)) u)))))
