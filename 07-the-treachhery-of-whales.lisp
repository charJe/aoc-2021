(cl:in-package #:advent-of-code)

(coalton-toplevel
  (define input
    (pipe
     "input/07-practice"
     read-lines
     head
     (fromsome "no input")
     (split-string ",")
     (map fromsome-parse-int)))

  (define (closest-line positions)
    (let ((goal
            (pipe
             (fold
              (fn (position frequencies)
                ())
              nil
              positions)
             (fold ))))
      (pipe
       positions
       (map (- goal))
       (map abs)
       (fold + 0))))
  )
