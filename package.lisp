(in-package #:cl-user)
(defpackage #:random-state
  (:nicknames #:org.shirakumo.random-state)
  (:use #:cl)
  (:shadow #:random)
  ;; generator.lisp
  (:export
   #:*generator*
   #:global-generator
   #:ensure-generator
   #:list-generator-types
   #:generator
   #:seed
   #:reseed
   #:next-byte
   #:bits-per-byte
   #:multivariate-p
   #:copy
   #:make-generator
   #:define-generator
   #:stateful-generator
   #:hash-generator
   #:index
   #:rewind)
  ;; protocol.lisp
  (:export
   #:draw
   #:random
   #:random-1d
   #:random-2d
   #:random-3d
   #:random-byte
   #:random-bytes
   #:random-sequence
   #:random-unit
   #:random-float
   #:random-int)
  ;; toolkit.lisp
  (:export
   #:hopefully-sufficiently-random-seed
   #:histogram
   #:print-histogram)
  ;; * rngs
  (:export
   #:adler32
   #:cityhash-64
   #:hammersley
   #:kiss11
   #:linear-congruence
   #:mersenne-twister-32
   #:mersenne-twister-64
   #:middle-square
   #:murmurhash3
   #:pcg
   #:quasi
   #:rc4
   #:sobol
   #:squirrel
   #:tt800
   #:xkcd
   #:xorshift-1024*
   #:xorshift-128
   #:xorshift-128+
   #:xorshift-32
   #:xorshift-64
   #:xorshift-64*
   #:xorwow
   #:xoshiro-128**
   #:xoshiro-128+
   #:xoshiro-256**
   #:xoshiro-256+))
