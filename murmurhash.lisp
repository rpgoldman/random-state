#|
 This file is a part of random-state
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.random-state)

(define-generator murmurhash3 32 (hash-generator) ()
  (:copy
   (make-murmurhash3 :%seed (murmurhash3-%seed generator)
                     :index (murmurhash3-index generator)))
  (:hash
   (declare (optimize speed (safety 1)))
   (flet ((rotl (x r)
            (fit-bits 32 (logior (ash x r) (ash x (- (- 32 r))))))
          (fmix (h)
            (update 32 h logxor (ash h -16))
            (update 32 h * #x85ebca6b)
            (update 32 h logxor (ash h -13))
            (update 32 h * #xc2b2ae35)
            (update 32 h logxor (ash h -16))))
     (declare (inline rotl fmix))
     (let ((h1 seed)
           (c1 #xcc9e2d51)
           (c2 #x1b873593)
           (k1 (fit-bits 32 index))
           (len 4))
       (update 32 k1 * c1)
       (update 32 k1 rotl 15)
       (update 32 k1 * c2)
       (update 32 h1 logxor k1)
       (update 32 h1 rotl 13)
       (setf h1 (fit-bits 32 (+ (* 5 h1) #xe6546b64)))
       (update 32 h1 logxor len)
       (fmix h1)))))
