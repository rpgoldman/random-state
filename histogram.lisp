(in-package #:org.shirakumo.random-state)

(defun histogram (rng bins &key (samples (floor 1e8)) (width 80) (stream *standard-output*) (print-summary T))
  (check-type samples (unsigned-byte 64))
  (let ((histogram (make-array bins))
        (sample-contribution (/ samples))
        (start (get-internal-real-time)))
    (format stream "0% ")
    (flet ((percentage (i)
             (round (* (- width 8) (/ i samples)))))
      (dotimes (i samples)
        (when (/= (percentage i) (percentage (1+ i)))
          (format stream "█"))
        (locally (declare (optimize speed))
          (incf (aref histogram (floor (* (random 1.0 rng) bins)))
                sample-contribution))))
    (format stream " 100%~%")
    (when print-summary
      (let ((duration (/ (- (get-internal-real-time) start)
                         INTERNAL-TIME-UNITS-PER-SECOND)))
        (format stream "Generation took: ~6,3fs, ~fμs/sample~%"
                duration (* 1000000 (/ duration samples)))))
    histogram))

(defun histogram-deviation (histogram)
  (loop for bin across histogram
        for deviation = (* 100 (- bin (/ (length histogram))))
        sum (abs deviation)))

(defun print-histogram (histogram &key (stream *standard-output*) (width 80))
  (assert (< 7 width))
  (let ((half-width (/ (- width 7) 2))
        (total 0.0))
    (loop for bin across histogram
          for deviation = (* 100 (- bin (/ (length histogram))))
          for chars = (max (- half-width) (min (+ half-width) (floor (* 2 half-width deviation))))
          do (format stream "~6,3@f% ~v@{░~}~v@{█~}~v@{░~}~%" deviation
                     (min half-width (+ half-width chars))
                     (abs chars)
                     (min half-width (- half-width chars))
                     NIL)
             (incf total (abs deviation)))
    (format stream "Cumulative deviation: ~6,3f%~%" total)
    histogram))

(defun histogram-all (&key (bins 5) (samples (floor 1e6)) (width 80) (stream *standard-output*))
  (format stream "Generating samples...~%")
  (let* ((stats (loop for rng in (list-generator-types)
                      for result = (ignore-errors
                                    (format stream "~&~19a " rng)
                                    (cons rng (histogram rng bins :samples samples :width (- width 20) :print-summary NIL)))
                      when result collect result)))
    (setf stats (sort stats #'< :key (lambda (x) (histogram-deviation (cdr x)))))
    (loop for (rng . histogram) in stats
          do (format stream "~&~%~a~%" rng)
             (print-histogram histogram :stream stream :width width))))

(defun benchmark (rng &key (samples 1e6) (stream *standard-output*))
  ;; this declaration is necessary because ENSURE-GENERATOR is a
  ;; forward-reference and SBCL does not like it that it misses the
  ;; chance to apply the compiler macro here. [2024/07/29:rpg]
  (let* ((rng (ensure-generator rng))
         (next-fun (next-byte-fun rng))
         (start (get-internal-run-time)))
    (declare (type (unsigned-byte 64) samples))
    (declare (type (function (generator) T) next-fun))
    (locally (declare (optimize speed (safety 0)))
      (loop repeat samples
            do (funcall next-fun rng)))
    (let* ((end (get-internal-run-time))
           (duration (float (/ (- end start) INTERNAL-TIME-UNITS-PER-SECOND) 0d0)))
      (format stream "Duration: ~12t~10,1f~%Samples: ~12t~10d~%Samples/s: ~12t~10,1f~%S/sample: ~12t~10,8f"
              duration samples (/ samples duration) (/ duration samples))
      (/ samples duration))))

(defun benchmark-all (&key (samples 1e6) (stream *standard-output*))
  (let* ((nullstream (make-broadcast-stream))
         (stats (loop for rng in (list-generator-types)
                      for result = (cons rng (handler-case (benchmark rng :samples samples :stream nullstream)
                                               (error () -1)))
                      when result collect result)))
    (setf stats (sort stats #'> :key #'cdr))
    (format stream "RNG~20tSamples/second~%")
    (loop for (rng . samples) in stats
          do (format stream "~&~a~20t~18,1f~%" rng samples))))
