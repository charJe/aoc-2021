(cl:in-package #:advent-of-code)

(coalton-toplevel
  (define-type (point :a)
    (point :a :a))

  (define-type (line :a)
    (line (point :a)
          (point :a)))

  (define (line-diagonal-p l)
    (match l
      ((line (point r1 c1)
             (point r2 c2))
       (and (/= r1 r2)
           (/= c1 c2)))))

  (define input
    (pipe
     "input/05"
     read-lines
     (map (fn (line)
            (map (compose (map fromsome-parse-int)
                          (split-string ","))
                 (split-string " -> " line))))
     (map (fn (row)
            (match row
              ((cons (cons x1 (cons y1 (nil)))
                     (cons (cons x2 (cons y2 (nil)))
                           (nil)))
               (line (point y1 x1) (point y2 x2))))))
     ;; (filter (compose not line-diagonal-p))
     ))

  (define board
    (repeat 1000 (repeat 1000 0)))

  (define (point-delta a b)
    (cond
      ((== a b) 0)
      ((< a b) 1)
      ((< b a) -1)))

  (define (%line-points points l)
    (match l
      ((line (point r1 c1) (point r2 c2))
       (if (and (== r1 r2) (== c1 c2))
           (cons (point r1 c1) points)
           (%line-points
            (cons (point r1 c1) points)
            (line (point (+ r1 (point-delta r1 r2))
                         (+ c1 (point-delta c1 c2)))
                  (point r2 c2)))))))

  (define (line-points l)
    (reverse (%line-points nil l)))

  (define (replace-by f i l)
    (let ((start (take i l))
          (rest (drop i l))
          (target (fromsome "no head" (head (take 1 rest))))
          (end (drop 1 rest)))
      (append start
              (cons (f target)
                    end))))

  (define (row-add-spot c row)
    (replace-by (+ 1) c row))

  (define (board-add-point p board)
    (match p
      ((point r c)
       (replace-by (row-add-spot c) r board))))

  (define (board-add-line l board)
    (pipe
     l line-points
     (fold board-add-point board)))

  (define (count-crossings lines)
    (pipe
     lines
     (fold board-add-line board)
     concat
     (filter (<= 2))
     length))
  )
