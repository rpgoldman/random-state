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
  (error () (uiop:quit 2)))

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
