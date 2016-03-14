#|
 This file is a part of random-state
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:random-state-viewer
  (:nicknames #:org.shirakumo.random-state.viewer)
  (:use #:cl+qt)
  (:export
   #:main))

(in-package #:org.shirakumo.random-state.viewer)
(in-readtable :qtools)

(defvar *generators* '(("Linear Congruence" . random-state::linear-congruence)
                       ("Mersenne Twister 32" . random-state::mersenne-twister-32)
                       ("Mersenne Twister 64" . random-state::mersenne-twister-64)
                       ("Middle Square" . random-state::middle-square)
                       ("RC4" . random-state::rc4)
                       ("TT800" . random-state::tt800)))

(define-widget viewer (QWidget)
  ((buffer :initform NIL :accessor buffer)))

(define-subwidget (viewer timer) (q+:make-qtimer viewer)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (floor (/ 1000 30))))

(define-slot (viewer tick) ()
  (declare (connected timer (timeout)))
  (q+:repaint viewer))

(define-override (viewer paint-event) (ev)
  (when buffer
    (with-finalizing ((painter (q+:make-qpainter viewer)))
      (q+:draw-image painter 0 0 buffer))))

(defmethod random-color ((generator random-state::generator))
  (let* ((gray (random-state::random-int generator 0 255))
         (a gray))
    (setf (ldb (byte 8 8) a) gray)
    (setf (ldb (byte 8 16) a) gray)
    a))

(defmethod generate ((viewer viewer) (generator random-state::generator))
  (let ((buffer (buffer viewer)))
    (unless (and buffer
                 (= (q+:width buffer) (q+:width viewer))
                 (= (q+:height buffer) (q+:height viewer)))
      (setf (buffer viewer) (q+:make-qimage (q+:width viewer) (q+:height viewer) (q+:qimage.format_RGB32)))
      (finalize buffer)
      (setf buffer (buffer viewer)))
    (dotimes (y (q+:height buffer))
      (dotimes (x (q+:width buffer))
        (setf (q+:pixel buffer x y) (random-color generator))))))

(define-widget main (QWidget)
  ())

(define-subwidget (main viewer) (make-instance 'viewer))

(define-subwidget (main regen) (q+:make-qpushbutton "Regenerate" main))

(define-subwidget (main chooser) (q+:make-qcombobox main)
  (q+:add-items chooser (mapcar #'car *generators*)))

(define-subwidget (main seed) (q+:make-qspinbox main)
  (setf (q+:maximum seed) #xFFFFFFF)
  (setf (q+:value seed) (mod (get-universal-time) #xFFFFFFF)))

(define-subwidget (main layout) (q+:make-qvboxlayout main)
  (q+:add-widget layout viewer)
  (let ((inner (q+:make-qhboxlayout)))
    (q+:add-widget inner chooser)
    (q+:add-widget inner seed)
    (q+:add-widget inner regen)
    (q+:add-layout layout inner)))

(define-slot (main regen) ()
  (declare (connected regen (clicked)))
  (let ((type (cdr (assoc (q+:current-text chooser) *generators* :test #'string-equal)))
        (seed (value seed)))
    (bt:make-thread
     (lambda ()
       (setf (q+:enabled regen) NIL)
       (generate viewer (random-state::make-generator type seed))
       (setf (q+:enabled regen) T)))))

(defun main ()
  (with-main-window (w 'main :main-thread NIL)))
