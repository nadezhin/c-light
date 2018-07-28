(in-package "ACL2")
(include-book "std/util/defrule" :dir :system)
(include-book "centaur/fty/top" :dir :system)

(include-book "tools/with-arith5-help" :dir :system)
(local (allow-arith5-help))

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
