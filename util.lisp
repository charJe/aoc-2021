(cl:in-package #:advent-of-code)

(coalton-toplevel
  (define (fromsome-parse-int x)
    (fromsome (concat-string x " is not an integer")
              (parse-int x))))
