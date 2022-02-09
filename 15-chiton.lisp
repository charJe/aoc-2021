(cl:in-package #:charje.advent-of-code)

(coalton-toplevel
  (define infinity (lisp integer () cl:most-positive-fixnum))

  (define-type point
    (point integer integer))

  (define-instance (eq point)
    (define (== p1 p2)
      (match p1
        ((point x1 y1)
         (match p2
           ((point x2 y2)
            (and (== x1 x2) (== y1 y2))))))))

  (define (+wrap a b)
    (pipe
     a (+ b) (flip - 1) (flip mod 9) (+ 1)))

  (define (read-board file)
    (pipe
     file read-lines
     (map (fn* (line)
            line (split-string "")
            (filter (/= ""))
            (map parse-int)
            (map (else 0))))
     25xbigger))

  (declare 5xbigger ((list (list integer)) -> (list (list (list integer)))))
  (define 5xbigger
    (fn* (board)
      (range 0 4)
      (map (fn* (addition)
             board (map (map (+wrap addition)))))))

  (define (25xbigger board)
    (pipe
     board
     5xbigger
     (map (fp 5xbigger reverse
            (reduce (zipwith append)) (else nil)))
     concat))

  (define (access-board r c board)
    (pipe
     (index board r) (else nil)
     (flip index c)))

  (define (point-neighbors p)
    (match p
      ((point r c)
       (make-list
        (point (+ r 1) c)
        (point r       (+ c 1))
        (point (- r 1) c)
        (point r       (- c 1))))))

  (define (min-dist ds vs)
    (pipe
     vs
     (map (fn* (v) (trie-ref v ds) (else infinity)))
     (zip vs)
     (reduce (fn* ((tuple minv min-value) (tuple v value))
               (if (< value min-value)
                   (tuple v value)
                   (tuple minv min-value))))
     (else (tuple (point 0 0) 0))
     tuple-0))

  (define (sure-path-cost board)
    (let* ((r-end board length (flip - 1))
           (c-end (index board 0) (else nil) length (flip - 1))
           (end (point r-end c-end))
           (cost (fn* ((point r c))
                   board (access-board r c) (else infinity))))
      (loop* ((dist (make-trie ((point 0 0) 0)))
              (vertexes
               (fold (fn* (r vs)
                       (fold (fn* (c vs)
                               (cons (point r c) vs))
                             vs (range 0 c-end)))
                     nil (range 0 r-end))))
        (if (null vertexes)
            (pipe dist (trie-ref end) (else 0))
            (let* ((u (min-dist dist vertexes) print))
              (if (== end u)
                  nil
                  (pipe vertexes (filter (/= u))))
              (recur
               (fold
                (fn* (v dist)
                  (let* ((alt dist (trie-ref u) (else infinity) (+ (cost v))))
                    (if (< alt (pipe (trie-ref v dist) (else infinity)))
                        (trie-set v alt dist)
                        dist)))
                dist
                (filter (flip member vertexes)
                        (point-neighbors u)))))))))
  )
