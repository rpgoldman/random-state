#|
 This file is a part of random-state
 (c) 2023 Robert P. Goldman and SIFT, LLC
 Author: Robert P. Goldman <rpgoldman@sift.net>
|#


(in-package :common-lisp-user)

(defpackage test-random-state
  (:use :common-lisp))

(in-package :test-random-state)

(load (merge-pathnames (make-pathname :directory '(:relative "quicklisp") :name "setup" :type "lisp")
                       (user-homedir-pathname)))

(push (namestring (uiop:pathname-directory-pathname *load-truename*)) ql:*local-project-directories*)

(handler-case
 (assert (uiop:pathname-equal (asdf:component-pathname (asdf:find-system "random-state"))
                              (uiop:pathname-directory-pathname *load-truename*)))
  (error () (uiop:die 2 "Not loading RANDOM-STATE from the expected location.")))

;;; check for clean build
(handler-bind
    ((error #'(lambda (c) (uiop:die 3 "Failed to build cleanly with error:~%~T~a" c))))
  (let ((asdf:*compile-file-failure-behaviour* :error)
        (asdf:*compile-file-warnings-behaviour* :error))
    (ql:quickload "random-state" :silent nil :verbose t)))

(ql:quickload "random-state-test")

(define-condition tests-failed (error)
  ()
  (:documentation "Include this condition so that we can detect failure of the test-op.")
  )

(defmethod parachute:summary :after ((report parachute:report))
  (call-next-method)
  (when (eq (parachute:status report) :failed)
    (signal 'tests-failed)))

(handler-case
    (asdf:test-system "random-state")
  (tests-failed ()  (uiop:quit 1)))
