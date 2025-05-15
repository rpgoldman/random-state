(in-package #:org.shirakumo.random-state)

(defvar *generator* *random-state*)

;; FIXME: these do not work correctly if RANDOM-BYTES fails because RANDOM-BYTE returns
;;        a non-integer, as it may do for pseudo generators.
(define-compiler-macro random-float (&whole whole generator from to &environment env)
  (cond ((and (constantp from env) (constantp to env))
         `(+ (load-time-value (min ,to ,from))
             (* (load-time-value (abs (- ,to ,from)))
                (scale-float (float (random-bytes ,generator (load-time-value (float-digits (- ,to ,from)))) ,from)
                             (load-time-value (- (float-digits (- ,to ,from))))))))
        ((constantp from env)
         `(let ((from ,from)
                (to ,to))
            (when (< to from)
              (rotatef from to))
            (+ from
               (* (- to from)
                  (scale-float (float (random-bytes ,generator (load-time-value (float-digits ,from))) ,from)
                               (load-time-value (- (float-digits ,from))))))))
        ((constantp to env)
         `(let ((from ,from)
                (to ,to))
            (when (< to from)
              (rotatef from to))
            (+ from
               (* (- to from)
                  (scale-float (float (random-bytes ,generator (load-time-value (float-digits ,to))) ,to)
                               (load-time-value (- (float-digits ,to))))))))
        (T
         whole)))

(define-compiler-macro random-unit (&whole whole generator &optional (type ''single-float) &environment env)
  (if (constantp type env)
      `(scale-float (coerce (random-bytes ,generator (load-time-value (float-digits (coerce 0 ,type)))) ,type)
                    (load-time-value (- (float-digits (coerce 0 ,type)))))
      whole))

(declaim (inline random))

(defun random (max &optional (generator *generator*))
  (macrolet ((gen (&rest types)
               `(etypecase max
                  ((integer 0)
                   (random-int generator 0 (1- max)))
                  ,@(loop for (type alias) in types
                          for zero = (coerce 0 type)
                          unless (and alias (subtypep type alias))
                          collect `((,type ,zero)
                                    (random-float generator ,zero max))))))
    (gen (short-float single-float)
         (single-float)
         (double-float)
         (long-float double-float))))

(defun draw (n &optional (generator *generator*))
  (let ((samples (make-array n :element-type 'single-float))
        (generator (ensure-generator generator)))
    (map-into samples (lambda () (random-unit generator)))))

(defun random-1d (generator index &optional (seed 0))
  (hash generator index seed))

(defun random-2d (generator x y &optional (seed 0))
  (hash generator (+ x (* y 198491317)) seed))

(defun random-3d (generator x y z &optional (seed 0))
  (hash generator (+ x (* y 198491317) (* z 6542989)) seed))

(defun random-byte (generator)
  (next-byte generator))

(declaim (ftype (function (T (integer 0)) (integer 0)) random-bytes))
(defun random-bytes (generator bits)
  (declare (optimize speed))
  (let* ((generator (ensure-generator generator))
         (chunk (bits-per-byte generator)))
    (declare (type (unsigned-byte 8) bits))
    (cond ((not (integerp chunk))
           (error "This generator does not output bits of randomness.~%  ~a~%generates~%  ~a"
                  generator chunk))
          ((= bits chunk)
           (next-byte generator))
          ((< bits chunk)
           (fit-bits bits (next-byte generator)))
          (T
           (let ((random 0))
             (declare (type (unsigned-byte 8) chunk))
             ;; Fill upper bits
             (loop for i downfrom (- bits chunk) above 0 by chunk
                   for byte = (next-byte generator)
                   do (setf (ldb (byte chunk i) random) byte))
             ;; Fill lowermost bits.
             ;; This will cause spilling on misaligned boundaries, but we don't care.
             (setf (ldb (byte chunk 0) random) (next-byte generator))
             random)))))

(defun random-sequence (generator sequence &key (start 0) (end (length sequence)))
  (let ((generator (ensure-generator generator)))
    (etypecase sequence
      (list
       (loop for cons on sequence
             do (setf (car cons) (next-byte generator))))
      (vector
       (loop for i from start below end
             do (setf (aref sequence i) (next-byte generator))))
      (sequence
       (map-into sequence (lambda () (next-byte generator)))))))

(defun random-unit (generator &optional (type 'single-float))
  (etypecase (bits-per-byte generator)
    ((member single-float double-float)
     (coerce (random-byte generator) type))
    (integer
     (let* ((bits (float-digits (coerce 0 type)))
            (random (random-bytes generator bits)))
       ;; KLUDGE: this sucks. Would be better if we could directly fill the mantissa.
       (scale-float (coerce random type) (- bits))))))

(defun random-float (generator from to)
  (let ((from from)
        (to to))
    (when (< to from)
      (rotatef from to))
    (+ from (* (- to from) (random-unit generator (type-of from))))))

(defun random-int (generator from to)
  (declare (optimize speed))
  (declare (type integer from to))
  (when (< to from)
    (rotatef from to))
  (let* ((generator (ensure-generator generator))
         (range (- to from))
         (bits (integer-length range)))
    (+ from
       ;; CANDIDATE is in the range [0, 2^BITS). Trying to map
       ;; this to our target range will introduce bias -- for example,
       ;; in the [0, 2] case, you could map 0->0, 1->0, 2->1, 3->2 and 0 would
       ;; come up twice as often as 1 & 2. So we instead re-roll until we get
       ;; a candidate in our range. Bigger range = fewer expected rolls.
       ;; But even in the 0..2 case, the expected number of rolls is still only ~1.33:
       ;;    E[R] = (3/4)1 + (1/4)(1 + E[R]),
       ;;    4E[R] = 3 + 1 + E[R],
       ;;    E[R] = 4/3 = ~1.33.
       (loop for candidate = (random-bytes generator bits)
             when (<= candidate range)
             return candidate))))

