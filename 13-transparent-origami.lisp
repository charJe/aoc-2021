(cl:in-package #:advent-of-code)

(coalton-toplevel
  (define-type point
    (point integer integer))

  (define (point-x p)
    (match p
      ((point x _) x)))

  (define (point-y p)
    (match p
      ((point _ y) y)))

  (define-instance (eq point)
    (define (== p1 p2)
      (match p1
        ((point x1 y1)
         (match p2
           ((point x2 y2)
            (and (== x1 x2) (== y1 y2))))))))

  (define-type paper-fold
    (fold-y integer)
    (fold-x integer))

  (define (read-points file)
    (pipe
     file read-lines
     (filter (compose not (string-starts-with/2 "fold")))
     (map (fn* (line)
            (let* (((cons x (cons y (nil))) line (split #\,) (filter (/= ""))))
              (point (pipe x parse-int (else 0))
                     (pipe y parse-int (else 0))))))))

  (define (read-folds file)
    (pipe
     file read-lines
     (filter (string-starts-with/2 "fold"))
     (map
      (fn* (line)
        line
        (split #\space) (filter (/= ""))
        (drop 2)
        head (else "")
        (split #\=) (filter (/= ""))))
     (map (fn* ((cons axis (cons spot (nil))))
            (let ((s (pipe spot parse-int (else 0))))
              (if (== axis "y")
                  (fold-y s)
                  (fold-x s)))))))

  (define (fold-point f p)
    (match p
      ((point x y)
       (match f
         ((fold-y spot)
          (point
           x (if (<= y spot)
                 y (- spot (- y spot)))))
         ((fold-x spot)
          (point
           (if (<= x spot)
               x (- spot (- x spot)))
           y))))))

  (define (origami points folds)
    (pipe
     folds
     (fold (fn* (f points)
             points (map (fold-point f))
             remove-duplicates)
           points)))

  (define (render points)
    (let* ((max-x points (map point-x) maximum (else 0) (+ 1))
           (max-y points (map point-y) maximum (else 0) (+ 1))
           (board (repeat max-y (repeat max-x "."))))
      points
      (fold (fn* ((point x y) board)
              (replace-by (replace-by (fn (_) "#") x)
                          y board))
            board)
      (map (join ""))
      (join #\newline)))
  )
