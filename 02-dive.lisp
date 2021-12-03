(cl:in-package #:advent-of-code)

(coalton-toplevel
  (define-type instruction
    (forward integer)
    (down integer)
    (up integer))

  (define-type (position :d :h :a)
    (position :d :h :a))

  (define input
    (pipe
     (lisp (list (list string)) ()
      (cl-list-to-coalton
       (cl:mapcar
         (cl:lambda (line)
           (cl-list-to-coalton (str:split " " line :omit-nulls cl:t)))
         (str:split #\newline (uiop:read-file-string "input/02") :omit-nulls cl:t))))
     (map (fn (line)
            (match line
              ((cons "forward" (cons x _))
               (forward (fromsome-parse-int x)))
              ((cons "down" (cons x _))
               (down (fromsome-parse-int x)))
              ((cons "up" (cons x _))
               (up (fromsome-parse-int x))))))))

  (define (execute-instructions current-position instructions)
    (let ((final-position
            (fold
             (fn (instruction current-position)
               (match current-position
                 ((position depth horizontal aim)
                  (match instruction
                    ((forward x)
                     (position (+ (* x aim)
                                  depth)
                               (+ horizontal x) aim))
                    ((down x)
                     (position depth horizontal (+ aim x)))
                    ((up x)
                     (position depth horizontal (- aim x)))))))
             current-position
             instructions)))
      (match final-position
        ((position depth horizontal _)
         (* depth horizontal)))))
  )
