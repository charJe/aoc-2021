(cl:in-package #:charje.advent-of-code)

(coalton-toplevel
  (define input
    (pipe
     "input/11"
     read-lines
     (map (fn-> (line)
            (the (list char) (into line))
            (map (fn-> (c)
                   c make-list into
                   fromsome-parse-int))))))

  (define board-size (length input))

  (define (tuple-board t)
    (match t
      ((tuple b _) b)))

  (define (tuple-count t)
    (match t
      ((tuple _ c) c)))

  (define (matrix-replace-by f r c matrix)
    (replace-by (replace-by f c)
                r matrix))

  (define (increment-energy r c board)
      (if (and (<= 0 r) (< r board-size)
               (<= 0 c) (< c board-size))
          (matrix-replace-by (+ 1) r c board)
          board))

  (define (try-flash row col board)
    (let-> ((spot (index board row) (else nil)
                  (flip index col) (else 0)))
      (if (or (< spot 10) (<= 100 spot))
          board
          (pipe
           (fold
            (fn (r board)
              (fold (increment-energy r)
                    board (range (- col 1) (+ col 1))))
            (matrix-replace-by (fn (_) 100) row col board)
            (range (- row 1) (+ row 1)))
           (flip
            (fold
             (fn (r board)
               (fold (try-flash r)
                     board (range (- col 1) (+ col 1)))))
            (range (- row 1) (+ row 1)))))))

  (define (model-step board--count)
    (let-> ((board board--count tuple-board
                   (map (map (+ 1))))
            (flashed (fold
                      (fn (r board)
                        (fold (try-flash r)
                              board (range 0 (- board-size 1))))
                      board (range 0 (- board-size 1))))
            (count flashed concat (countby (< 9))
                   (+ (tuple-count board--count))))
      (tuple
       (map (map (fn (oct)
                   (if (< 9 oct) 0 oct)))
            flashed)
       count)))

  (define (model-flashes board)
    (pipe
     (range 1 195)
     (fold
      (fn (_ t)
        (model-step t))
      (tuple board 0))))

  (define (first-all-flash board)
    (let-> ((%all-flash
             (fn (n board--count)
               (match-> (model-step board--count)
                 ((tuple board _)
                  (if (pipe board concat (all (== 0)))
                      n
                      (%all-flash (+ n 1) (tuple board 0))))))))
      (%all-flash 1 (tuple board 0))))
  )
