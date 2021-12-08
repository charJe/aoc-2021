(in-package :cl-user)
(mapc 'require (list :coalton :str :funds))

(defpackage advent-of-code
  (:use #:coalton #:coalton-library))
