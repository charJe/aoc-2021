(in-package :cl-user)
(mapc 'require (list :coalton :str))

(defpackage advent-of-code
  (:use #:coalton #:coalton-library))
