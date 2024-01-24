(in-package #:org.shirakumo.random-state)

(define-generator xkcd 32 (generator) ()
  (:reseed)
  (:next
   (declare (optimize speed (safety 0)))
   ;; Chosen fairly by dice roll.
   ;; Guaranteed to be random.
   4))
