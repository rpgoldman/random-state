(in-package #:org.shirakumo.random-state)

(define-generator quasi 'single-float (stateful-generator)
    ((source *generator* :type generator)
     (last 0.0 :type single-float))
  (:reseed
   (reseed source seed)
   (setf last 0.0))
  (:next
   (setf last (mod (+ last 0.5 (random-unit source)) 1.0))))

;; TODO: multivariate
