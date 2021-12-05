(cl:in-package #:advent-of-code)

(coalton-toplevel
  (define (read-lines filename)
    (lisp (list string) (filename)
       (str:split #\newline
                  (uiop:read-file-string filename)
                  :omit-nulls cl:t)))

  (define (split-string d s)
    (lisp (list string) (d s)
       (str:split d s :omit-nulls cl:t)))

  (define (fromsome-parse-int x)
    (fromsome (concat-string x " is not an integer")
              (parse-int x)))
  )
