(cl:in-package #:advent-of-code)

(coalton-toplevel
  ;; 0 becomes -1 and 1 becomes 1
  (define input
    (pipe
     (read-lines "input/03")
     (map
      (fn (line)
        (map (fn (char)
               (if (== char #\1) 1 -1))
             (unpack-string line))))))

  (define (binary-to-decimal nums)
    (pipe
     (zipwith
      (fn (place value)
        (* value (expt 2 place)))
      (range 0 (- (length nums) 1))
      (reverse nums))
     (fold + 0)))

  (define (power-consumption input)
    (let ((gamma
            (pipe
             input
             (fold (fn (row sum)
                     (zipwith + row sum))
                   (repeat (length input) 0))
             (map (fn (num)
                    (if (< 0 num) 1 0)))))
          (epsilon
            (map (fn (num)
                   (if (== 0 num) 1 0))
                 gamma)))
      (* (binary-to-decimal gamma)
         (binary-to-decimal epsilon))))
  )
