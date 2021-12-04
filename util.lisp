(cl:in-package #:advent-of-code)

(coalton-toplevel
  (define (read-lines filename)
    (lisp (list string) (filename)
      (cl-list-to-coalton
       (str:split #\newline
                  (uiop:read-file-string filename)
                  :omit-nulls cl:t))))

  (define (fromsome-parse-int x)
    (fromsome (concat-string x " is not an integer")
              (parse-int x))))
