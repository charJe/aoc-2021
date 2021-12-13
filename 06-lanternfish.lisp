(cl:in-package #:charje.advent-of-code)

(coalton-toplevel
  (define birthing 0)
  (define mother 6)
  (define baby 8)

  (define input
    (pipe
     "input/06"
     read-lines head (fromsome "no input")
     (split-string ",")
     (map fromsome-parse-int)
     (fold
      (fn (age fishs)
          (replace-by (+ 1) age fishs))
      (repeat (+ baby 1) 0))))

  (define (simulate days fishs)
   (if (== 0 days)
       (sum fishs)
       (simulate
        (- days 1)
        (let ((babies (fromsome "no head" (head fishs))))
          (replace-by (+ babies) mother
                      (append (drop 1 fishs)
                              (make-list babies)))))))
)
