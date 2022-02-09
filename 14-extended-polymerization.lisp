(cl:in-package #:charje.advent-of-code)

(coalton-toplevel
  (define (pair-frequencies chars)
    (loop* ((chars chars)
            (fs empty-hash))
      (match chars
        ((cons x1 (cons x2 xs))
         (recur (cons x2 xs) (hash-replace-by 0 (+ 1) (tuple x1 x2) fs)))
        (_ fs))))

  (declare read-template (string -> (list char)))
  (define (read-template file)
    (pipe
     file read-lines
     head (else "")
     into))

  (define (read-insertion-mappipng file)
    (pipe
     file read-lines
     (drop 1)
     (fold (fn* (line insertions-instructions)
             (let* (((cons pair (cons insertion (nil))) line (split-string " -> ") (filter (/= ""))))
               insertions-instructions
               (hash-set (pipe pair (split-string "") (filter (/= ""))
                               (map (fn* (p) p into head (else #\0)))
                               (fn* ((cons left (cons right (nil))))
                                 (tuple left right)))
                         (pipe insertion into head (else #\0)))))
           empty-hash)))

  (define (polymerize chars insertion-mapping)
    (loop* ((iterations-left 40)
            (pair-fs (pair-frequencies chars))
            (character-fs (frequencies chars)))
      (if (== 0 iterations-left)
          character-fs
          (let* ((new-tripples
                  pair-fs
                  (map-hash
                   (fn* ((tuple left right) num)
                     (match (hash-ref (tuple left right) insertion-mapping)
                       ((none) none)
                       ((some c) (some (tuple (tuple3 left c right) num))))))
                  (filter (/= none))
                  (map (else (tuple (tuple3 #\0 #\0 #\0) 0)))))
            (recur (- iterations-left 1)
                   (fold (fn* ((tuple (tuple3 left mid right) num) p-fs)
                           p-fs
                           (hash-replace-by 0 (+ num) (tuple left mid))
                           (hash-replace-by 0 (+ num) (tuple mid right)))
                          (fold (fn* ((tuple (tuple3 left _ right) _))
                                  (hash-set (tuple left right) 0))
                                pair-fs new-tripples)
                         new-tripples)
                    (fold (fn* ((tuple (tuple3 _ mid _) num) c-fs)
                            (hash-replace-by 0 (+ num) mid c-fs))
                          character-fs new-tripples))))))
  
  (define (rate-polymer frequencies)
    (let* ((keys-values (map-hash tuple frequencies))
           (most (fn* (op)
                   keys-values
                   (reduce
                    (fn* ((tuple least-frequent-letter smallest-frequency)
                          (tuple candidate-letter candidate-frequency))
                      (if (op candidate-frequency smallest-frequency)
                          (tuple candidate-letter candidate-frequency)
                          (tuple least-frequent-letter smallest-frequency))))
                   (else (tuple #\0 0))
                   tuple-1))
           (least-frequent (most <))
           (most-frequent (most >)))
      (- most-frequent least-frequent)))
  )
