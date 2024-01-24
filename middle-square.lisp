(in-package #:org.shirakumo.random-state)


(define-generator middle-square (middle-square-bits generator) (stateful-generator)
    ((bits 64)
     (state 0))
  (:reseed
   (setf state seed)
   (setf bits (integer-length seed)))
  (:next
   (let* ((square (expt state 2))
          (offset (floor (max 0 (- (integer-length square) bits)) 2))
          (new (ldb (byte bits offset) square)))
     (setf state new))))
