(in-package #:org.shirakumo.random-state)

(define-generator linear-congruence 64 (stateful-generator)
  ((state 0 :type (unsigned-byte 64))
   (multiplier 6364136223846793005 :type (unsigned-byte 64))
   (increment 1442695040888963407 :type (unsigned-byte 64)))
  (:reseed
   (setf state (mod seed (1- (ash 1 64)))))
  (:next
   (declare (optimize speed))
   (update 64 state * multiplier)
   (update 64 state + increment)))
