(in-package #:org.shirakumo.random-state)

(define-generator quasi 'single-float (stateful-generator)
    ((source *generator* :type T)
     (last 0.0 :type single-float))
  (:reseed
   (reseed source seed)
   (setf last 0.0))
  (:next
   (declare (optimize speed (safety 1)))
   (setf last (mod (+ last 0.5 (random-unit source)) 1.0))))

;; TODO: multivariate
