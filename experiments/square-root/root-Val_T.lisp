(in-package "ACL2")
(include-book "std/util/defrule" :dir :system)
(include-book "centaur/fty/top" :dir :system)

(include-book "tools/with-arith5-help" :dir :system)
(include-book "ceil-root")
(include-book "Val_T-theory")
(local (allow-arith5-help))

(local
 (with-arith5-nonlinear-help
  (defruled root_delta_T-lemma
    (implies (and (real/rationalp x)
                  (<= x (expt (sup_T) 2)))
             (<= (* (delta_T) (ceil-root (* (expt (delta_T) -2) x)))
                 (sup_T)))
    :use ((:instance ceil-root-monotone
                     (x (* (expt (delta_T) -2) x))
                     (y (* (expt (delta_T) -2) (expt (sup_T) 2))))
          (:instance ceil-root-sqr
                     (x (/ (sup_T) (delta_T))))))))

(with-arith5-help
 (define root_delta_T
   ((x real/rationalp))
   :returns (result Val_T-p
                    :hints (("goal" :in-theory (enable Val_T-p)
                             :use (:instance root_delta_T-lemma
                                             (x (min (realfix x) (expt (sup_T) 2)))))))
   (b* ((x (min (realfix x) (expt (sup_T) 2))))
     (* (delta_T) (ceil-root (* x (expt (delta_T) -2)))))
   ///
   (fty::deffixequiv root_delta_T)
   (with-arith5-nonlinear-help
    (defrule root_delta_T-lower-bound
     (b* ((root1 (- (root_delta_T x) (delta_T))))
       (implies (and (real/rationalp x)
                     (< 0 x))
                (< (expt root1 2) x)))
     :use (:instance ceil-root-lower-bound
                     (x (* (min x (expt (sup_T) 2)) (expt (delta_T) -2))))))
   (with-arith5-nonlinear-help
    (defrule root_delta_T-upper-bound
      (b* ((root (root_delta_T x)))
        (implies (and (real/rationalp x)
                      (<= x (expt (sup_T) 2)))
                 (<= x (expt root 2))))
      :use (:instance ceil-root-upper-bound
                      (x (* x (expt (delta_T) -2))))))
   (defruled root_delta_T-default-<
     (implies (<= (realfix x) 0)
              (equal (root_delta_T x) 0))
     :enable ceil-root-default)
   (defruled root_delta_T-default->
     (implies (> (realfix x) (expt (sup_T) 2))
              (equal (root_delta_T x) (sup_T)))
     :use (:instance ceil-root-sqr
                     (x (/ (sup_T) (delta_T)))))))
