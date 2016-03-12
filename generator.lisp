#|
This file is a part of random-state
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.random-state)

(defclass generator ()
  ((seed :initarg :seed :reader seed :writer %set-seed))
  (:default-initargs
   :seed (error "SEED required.")))

(defmethod print-object ((generator generator) stream)
  (print-unreadable-object (generator stream :type T)
    (format stream "~s" (seed generator))))

(defgeneric random-float (generator from to))

(defgeneric random-int (generator from to))

(defgeneric reseed (generator &optional new-seed))

(defmethod reseed :around ((generator generator) &optional new-seed)
  (declare (ignore new-seed))
  (call-next-method)
  generator)

(defvar *generator*)

(defun make-generator (type &optional seed)
  (let ((generator (make-instance type :seed seed)))
    (reseed generator seed)))

(defun random (limit &optional (generator *generator*))
  (random-int generator 0 limit))
