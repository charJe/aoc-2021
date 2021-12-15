(cl:in-package #:charje.advent-of-code)

(coalton-toplevel
  (declare input (list (list char)))
  (define input
    (pipe
     "input/10"
     read-lines
     (map into)))

  (define (char-syntax-score c)
    (match c
      (#\right_parenthesis 3)
      (#\right_square_bracket 57)
      (#\right_curly_bracket 1197)
      (#\greater-than_sign 25137)
      (_ 0)))

  (define (char-complete-score c)
    (match c
      (#\right_parenthesis 1)
      (#\right_square_bracket 2)
      (#\right_curly_bracket 3)
      (#\greater-than_sign 4)
      (_ 0)))

  (define (matching-char c)
    (match c
      (#\right_parenthesis #\left_parenthesis)
      (#\left_parenthesis #\right_parenthesis)
      (#\right_square_bracket #\left_square_bracket)
      (#\left_square_bracket #\right_square_bracket)
      (#\right_curly_bracket #\left_curly_bracket)
      (#\left_curly_bracket #\right_curly_bracket)
      (#\greater-than_sign #\less-than_sign)
      (#\less-than_sign #\greater-than_sign)
      (_ #\0)))

  (define (opening-char c)
    (member
     c (make-list
        #\left_parenthesis
        #\left_square_bracket
        #\left_curly_bracket
        #\less-than_sign)))
  
  (define (find-corrupt chars)
    (let ((%find-corrupt
            (fn (stack chars)
              (match chars
                ((nil) none)
                ((cons x xs)
                 (if (opening-char x)
                     (%find-corrupt (cons x stack) xs)
                     (let-> ((top stack head (else #\0)))
                       (if (== top (matching-char x))
                           (%find-corrupt (drop 1 stack) xs)
                           (some x)))))))))
      (%find-corrupt nil chars)))
  
  (define (error-score lines)
    (pipe
     lines
     (map find-corrupt)
     (map (else #\0))
     (map char-syntax-score)
     sum))

  (define (line-ending chars)
    (let ((%find-ending
            (fn (stack chars)
              (match-> chars
                ((nil)
                 stack (map matching-char))
                ((cons x xs)
                 (if (opening-char x)
                     (%find-ending (cons x stack) xs)
                     (let-> ((top stack head (else #\0)))
                       (if (== top (matching-char x))
                           (%find-ending (drop 1 stack) xs)
                           (error "corrupt line")))))))))
      (%find-ending nil chars)))

  (define (line-complete-score line)
    (pipe
     line line-ending
     (fold (fn-> (c acc)
             c char-complete-score
             (+ (* 5 acc)))
           0)))
  
  (define (complete-score lines)
    (let-> ((scores lines
                    (filter (fn (line)
                              (match (find-corrupt line)
                                ((some _) false)
                                (_ true))))
                    (map line-complete-score)
                    sort))
      (index scores (floor/ (length scores) 2))
      (else 0)))
  )
