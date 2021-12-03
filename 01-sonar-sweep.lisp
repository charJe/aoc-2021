(cl:in-package #:advent-of-code)

(coalton-toplevel
  (define input
    (pipe
     (lisp (list string) ()
       (cl-list-to-coalton
        (str:split #\newline
                   (uiop:read-file-string "input/01")
                   :omit-nulls cl:t)))
     (map parse-int)
     (map (fromsome "not an integer"))))

  (define (differences input)
    (pipe
     (zip (append (match (tail input)
                   ((none) nil)
                   ((some x) x))
                  (make-list 0))
          (take (- (length input) 1) input))
     (fold (fn (depths increases)
             (let ((previous (fst depths))
                   (next (snd depths)))
               (if (<= previous next)
                   increases
                   (+ increases 1))))
           0)))

  (define (make-windows input)
    (let ((window (take 3 input)))
      (if (/= 3 (length window))
          nil
          (cons (fold + 0 window)
                (make-windows (drop 1 input))))))
  )

