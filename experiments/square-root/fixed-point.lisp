(in-package "ACL2")

(include-book "std/util/defrule" :dir :system)
(include-book "centaur/fty/top" :dir :system)
(include-book "tools/with-arith5-help" :dir :system)

(include-book "Val_T-theory")
(include-book "root-Val_T")
(local (allow-arith5-help))

(encapsulate
  (((root *) => *))

  (local (defun root (x) (root_delta_T x)))

  (with-arith5-help
   (defrule root-constraint
     (implies (Arg_Stp-p v)
              (and (Val_T-p (root v))
                   (>= (* (root v) (root v)) v)
                   (< (expt (- (root v) (delta_T)) 2) v)))
     :enable Arg_Stp-p
     :use ((:instance root_delta_T-lower-bound
                      (x v))
           (:instance root_delta_T-upper-bound
                      (x v))))))
