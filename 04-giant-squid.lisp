(cl:in-package #:advent-of-code)

(coalton-toplevel
  (define-type (bingo-spot :a)
    (marked :a)
    (unmarked :a))

  (define-type bingo-board
    (list (list (bingo-spot integer))))

  (define input
    (read-lines "input/04"))

  (define draws
    (match (head input)
      ((none) nil)
      ((some line)
       (pipe
        line
        (split-string ",")
        (map fromsome-parse-int)))))

  (define (read-boards lines)
    (match lines
      ((nil) nil)
      (_
       (pipe
        lines
        (take 5)
        (map (compose (map (compose unmarked
                                    fromsome-parse-int))
                      (split-string " ")))
        (flip cons (read-boards (drop 5 lines)))))))

  (define boards
    (read-boards (drop 1 input)))

  (define (bingo-board-winner-p board)
    (any
     (fn (row)
       (all
        (fn (spot)
          (match spot
            ((marked _) true)
            ((unmarked _) false)))
        row))
     (append board (transpose board))))

  (declare bingo-spot-mark
           (integer -> (bingo-spot integer)
                    -> (bingo-spot integer)))
  (define (bingo-spot-mark n spot)
    (match spot
      ((marked _) spot)
      ((unmarked a)
       (if (== n a)
           (marked a)
           spot))))

  (declare bingo-board-mark
           (integer -> (list (list (bingo-spot integer)))
                    -> (list (list (bingo-spot integer)))))
  (define (bingo-board-mark n board)
    (map (fn (row)
           (map (bingo-spot-mark n) row))
         board))

  (define (bingo-board-score d board)
    (pipe
     board
     (concatmap
      (fn (row)
        (map
         (fn (spot)
           (match spot
             ((marked _) 0)
             ((unmarked a) a)))
         row)))
     (fold + 0)
     (* d)))

  (define (winner-score draws boards)
    (match draws
      ((nil) 0)
      ((cons draw draws)
       (let ((marked-boards (map (bingo-board-mark draw) boards))
             (winners (filter bingo-board-winner-p marked-boards)))
         (match winners
           ((cons winner _)
            (bingo-board-score draw winner))
           ((nil)
            (winner-score draws marked-boards)))))))

  (define (last-score draws boards)
    (match draws
      ((nil) 0)
      ((cons draw draws)
       (let ((marked-boards (map (bingo-board-mark draw) boards))
             (not-winners (filter (compose not bingo-board-winner-p)
                                  marked-boards)))
         (match not-winners
           ((nil) 0)
           ((cons _ (nil))
            (winner-score draws not-winners))
           (_
            (last-score draws marked-boards)))))))
  )
