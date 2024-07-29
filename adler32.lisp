(in-package #:org.shirakumo.random-state)

(define-generator adler32 32 (hash-generator) ()
  (:copy
   (make-adler32 :%seed (adler32-%seed generator)
                 :index (adler32-index generator)))
  (:hash
   (declare (optimize speed (safety 1)))
   (let ((s1 1)
         (s2 0)
         (bits (fit-bits 32 (+ seed index))))
     (macrolet ((it (byte)
                  `(setf s1 (fit-bits 32 (mod (+ s1 ,byte) 65521))
                         s2 (fit-bits 32 (mod (+ s2 s1) 65521)))))
       (it (ldb (byte 8 0) bits))
       (it (ldb (byte 8 8) bits))
       (it (ldb (byte 8 16) bits))
       (it (ldb (byte 8 24) bits)))
     (logior (ash s2 16) s1))))
