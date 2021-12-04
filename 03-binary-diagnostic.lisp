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

  (define part2-input
    (pipe
     input
     (map
      (fn (row)
        (map (fn (num)
               (if (== -1 num) 0 1))
             row)))))

  (define (rating decider i input)
    (match (tail input)
      ((none) (make-list 0))
       ;; only one element left
      ((some (nil)) (fromsome "no head" (head input)))
      ((some _)
       (let ((decision
               (pipe
                input
                (map (fn (row)
                       (match (index row i)
                         ((none) -1)
                         ((some x) x))))
                decider)))
         (pipe
          input
          (filter (fn (row)
                    (pipe
                     row
                     (flip index i)
                     (fromsome "index out of range")
                     (== decision))))
          (rating decider (+ i 1)))))))

  (define (decider comparison nums)
    (match (pipe
            nums
            (filter (/= -1))
            (partition (== 0)))
      ((tuple zeros ones)
       (if (comparison
            (length zeros)
            (length ones))
           1 0))))

  (define (oxygen-rating input)
    (binary-to-decimal
     (rating (decider <=) 0 input)))

  (define (scrubber-rating input)
    (binary-to-decimal
     (rating (decider >) 0 input)))

  (define (life-support input)
    (* (oxygen-rating input)
       (scrubber-rating input)))
  )
