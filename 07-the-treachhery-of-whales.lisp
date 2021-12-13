(cl:in-package #:charje.advent-of-code)

(coalton-toplevel
  (define input
    (pipe
     "input/07"
     read-lines
     head
     (fromsome "no input")
     (split-string ",")
     (map fromsome-parse-int)))

  (define (+to0 num)
    (let ((%+to0
            (fn (a num)
              (if (== 0 num)
                  a
                  (%+to0 (+ a num) (- num 1))))))
      (%+to0 0 num)))

  (declare fuel-requried ((list integer) -> integer -> integer))
  (define (fuel-requried positions spot)
    (pipe
     positions
     (map (compose +to0 (compose abs (- spot))))
     sum))

  (declare closest-line ((list integer) -> integer))
  (define (closest-line positions)
    (let ((fuel-req (fuel-requried positions))
          (%closest
            (fn (current-min low low-value high high-value)
              (if (<= (- high 1) low)
                  (min low-value high-value)
                  (let ((mid (floor/ (+ low high) 2))
                        (mid-value (fuel-req mid))
                        (smallest (else 0 (minimum (make-list current-min low-value high-value)))))
                    (cond
                      ((< high-value low-value)
                       (%closest
                        smallest
                        mid mid-value
                        high high-value))
                      ((<= low-value high-value)
                       (%closest
                        smallest
                        low low-value
                        mid mid-value))))))))
      (let ((highest (else 0 (maximum positions)))
            (lowest (else 0 (minimum positions)))
            (lowest-fuel (fuel-req lowest)))
        (%closest lowest-fuel
                  lowest lowest-fuel
                  highest (fuel-req highest)))))
  )
