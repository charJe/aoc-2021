(cl:in-package #:charje.advent-of-code)

(coalton-toplevel
  (define-type point
    (point integer integer))

  (define-instance (eq point)
    (define (== p1 p2)
      (match p1
        ((point x1 y1)
         (match p2
           ((point x2 y2)
            (and (== x1 x2)
                 (== y1 y2))))))))
  (define input
    (pipe
     "input/09"
     read-lines
     (map (fn-> (line)
            (the (list char) (into line))
            (map (fn-> (c)
                   c make-list into
                   fromsome-parse-int))))))

  (define (access-board board r c)
    (pipe
     (index board r) (else nil)
     (flip index c) (else 9)))

  (define (neighbors board r c)
    (let ((ac (access-board board)))
      (make-list
       (ac (+ r 1) c)
       (ac r (+ c 1))
       (ac (- r 1) c)
       (ac r (- c 1)))))

  (define (neighboring-points p)
    (match p
      ((point r c)
       (make-list
       (point (+ r 1) c)
       (point r (+ c 1))
       (point (- r 1) c)
       (point r (- c 1))))))

  (define (basins board)
    (pipe
     board
     (map-index
      (fn-> (row r)
        row (map-index
             (fn-> (_ c)
               (let ((spot (access-board board r c)))
                 (if (all (< spot)
                        (neighbors board r c))
                     (some (point r c))
                     none))))))
     concat
     (filter (fn (x)
               (match x
                 ((some _) true)
                 (_ false))))
     (map (else (point 0 0)))))

  (define (risk board)
    (pipe
     board basins
     (map (fn (p)
            (match p
              ((point r c)
               (access-board board r c)))))
     (map (+ 1))
     sum))

  (define (basin-size board p)
    (let-> ((basin-points
             (fn (points board p)
               (match-> p
                 ((point r c)
                  (if (<= 9 (access-board board r c))
                      (tuple board points)
                      (pipe
                       p neighboring-points
                       (fold
                        (fn (p board--points)
                          (match board--points
                            ((tuple board points)
                             (basin-points
                              points
                              (replace-by
                               (replace-by (fn (_) 9) c)
                               r board)
                              p))))
                        (tuple board (cons p points))))))))))
      (match-> (basin-points nil board p)
        ((tuple _ points)
         points
         remove-duplicates
         length))))

  (define (largest-basins board)
    (pipe
     board basins
     (map (basin-size board))
     sort reverse
     (take 3)
     (reduce *) (else 0)))
  )
