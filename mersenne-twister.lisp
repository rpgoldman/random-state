#|
 This file is a part of random-state
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.random-state)

(defclass mersenne-twister (generator)
  ((w)
   (n)
   (m)
   (r)
   (a)
   (b)
   (c)
   (s)
   (t)
   (u)
   (d)
   (l)))
