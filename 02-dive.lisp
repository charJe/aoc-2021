(cl:in-package #:advent-of-code)

(coalton-toplevel
  (define-type instruction
    (forward integer)
    (down integer)
    (up integer))

  (define input
    (pipe
     (lisp (list (list string)) ()
      (cl-list-to-coalton
       (cl:mapcar
         (cl:lambda (line)
           (cl-list-to-coalton (str:split " " line :omit-nulls cl:t)))
         (str:split #\newline (uiop:read-file-string "input/02-practice") :omit-nulls cl:t))))
     (map (fn (line)
            (match line
              ((cons "forward" (cons x xs))
               (forward (fromsome "" (parse-int x))))
              ((cons "down" (cons x xs))
               (down (fromsome "" (parse-int x))))
              ((cons "up" (cons x xs))
               (up (fromsome ""  (parse-int x)))))))))
  )
