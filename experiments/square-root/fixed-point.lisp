(in-package "ACL2")

(include-book "std/util/defrule" :dir :system)
(include-book "centaur/fty/top" :dir :system)

(include-book "Val_T-theory")
(include-book "root-linear")

(define root_delta_T
    ((x rationalp))
    :returns (result rationalp)
    (b* ((x (rfix x)))
        (root-linear x (delta_T)))
    ///
    (fty::deffixequiv root_delta_T))

(defrule root_delta_T-in-grid
    (implies
        (Arg_Stp-p x)
        (integerp
            (*
                (/ (delta_T))
                (root_delta_T x)
            )
        )
    )
    :enable (root_delta_T Arg_Stp-p
             integer-1-delta_T delta_T-constraint)
    :use (:instance root-linear-in-grid
             (x x) (d (delta_T)))
)

(defrule root_delta_T-lower-bound
    (implies
        (Arg_Stp-p x)
        (<= (- (inf_T)) (root_delta_T x))
    )
    :enable (root_delta_T Arg_Stp-p
             Val_T-p inf_T-constraint)
    :use (:instance root-linear-bound
             (x x) (d (delta_T)))
)

(defrule root_delta_T-upper-bound
    (implies
        (Arg_Stp-p x)
        (<= (root_delta_T x) (sup_T))
    )
    :enable (root_delta_T Arg_Stp-p Val_T-p
             integer-1-delta_T sup_T-constraint)
    :use (:instance root-linear-s-bound
             (x x) (d (delta_T)) (s (sup_T)))
)

(defrule Val_T-root_delta_T
    (implies
        (Arg_Stp-p v)
        (Val_T-p (root_delta_T v))
    )
    :enable Val_T-p
    :use ((:instance root_delta_T-in-grid
            (x v))
          (:instance root_delta_T-lower-bound
            (x v))
          (:instance root_delta_T-upper-bound
            (x v)))
)

(defrule root_delta_T-bounded
    (implies
        (Arg_Stp-p v)
        (>= (* (root_delta_T v) (root_delta_T v)) v)
    )
    :enable (root_delta_T Arg_Stp-p
             Val_T-p delta_T-constraint)
    :use ((:instance root-linear-upper-bound
            (x v) (d (delta_T))))
)

(defrule root_delta_T-bound
    (implies
        (Arg_Stp-p v)
        (< (expt (- (root_delta_T v) (delta_T)) 2) v)
    )
    :enable (root_delta_T Arg_Stp-p
             Val_T-p delta_T-constraint)
    :use ((:instance root-linear-lower-bound
            (x v) (d (delta_T))))
)

(encapsulate
  (((root *) => *))

  (local (defun root (x) (root_delta_T x)))

  (defrule root-constraint
     (implies (Arg_Stp-p v)
              (and (Val_T-p (root v))
                   (>= (* (root v) (root v)) v)
                   (< (expt (- (root v) (delta_T)) 2) v)))
     :use (Val_T-root_delta_T
           root_delta_T-bounded
           root_delta_T-bound)))
