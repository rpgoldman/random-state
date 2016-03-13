#|
 This file is a part of random-state
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:random-state-viewer
  (:nicknames #:org.shirakumo.random-state.viewer)
  (:use #:cl+qt))

(in-package #:org.shirakumo.random-state.viewer)
(in-readtable :qtools)

(defvar *generators* '(("Linear Congruence" . random-state::linear-congruence)
                       ("Mersenne Twister 32" . random-state::mersenne-twister-32)
                       ("Mersenne Twister 64" . random-state::mersenne-twister-64)
                       ("Middle Square" . random-state::middle-square)
                       ("TT800" . random-state::tt800)))

(define-widget viewer (QWidget)
  ((buffer :initform NIL :accessor buffer)))

(define-initializer (viewer setup)
  )

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
  (let ((buffer (q+:make-qimage (q+:width viewer) (q+:height viewer) (q+:qimage.format_RGB32)))
        (old (buffer viewer)))
    (dotimes (x (q+:width buffer))
      (dotimes (y (q+:height buffer))
        (setf (q+:pixel buffer x y) (random-color generator))))
    (setf (buffer viewer) buffer)
    (finalize old)))

(define-widget main (QWidget)
  ())

(define-subwidget (main viewer) (make-instance 'viewer))

(define-subwidget (main regen) (q+:make-qpushbutton "Regenerate" main))

(define-subwidget (main chooser) (q+:make-qcombobox main)
  (q+:add-items chooser (mapcar #'car *generators*)))

(define-subwidget (main seed) (q+:make-qspinbox main)
  (setf (q+:maximum seed) (1- (expt 2 8)))
  (setf (q+:value seed) (mod (get-universal-time) (1- (expt 2 8)))))

(define-subwidget (main layout) (q+:make-qvboxlayout main)
  (q+:add-widget layout viewer)
  (let ((inner (q+:make-qhboxlayout)))
    (q+:add-widget inner chooser)
    (q+:add-widget inner seed)
    (q+:add-widget inner regen)
    (q+:add-layout layout inner)))

(define-slot (main regen) ()
  (declare (connected regen (clicked)))
  (setf (q+:enabled regen) NIL)
  (generate viewer (random-state::make-generator (cdr (assoc (q+:current-text chooser) *generators* :test #'string-equal))
                                                 (value seed)))
  (q+:repaint viewer)
  (setf (q+:enabled regen) T))

(defun main ()
  (with-main-window (w 'main :main-thread NIL)))
