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
  (defrule ceil-root-default
    (implies (or (not (real/rationalp x))
                 (<= x 0))
             (equal (ceil-root x) 0))
    :enable ceil-root-aux))
