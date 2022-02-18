(cl:in-package #:charje.advent-of-code)

(coalton-toplevel
  (declare pair-frequencies ((list char) -> (trie (tuple char char) integer)))
  (define (pair-frequencies chars)
    "Count Frequencies of pairs of characters (overlapping)."
    (loop* ((chars chars)
            (fs empty-trie))
      (match chars
        ((cons x1 (cons x2 xs))
         (recur (cons x2 xs) (trie-replace-by 0 (+ 1) (tuple x1 x2) fs)))
        (_ fs))))

  (declare read-template (string -> (list char)))
  (define (read-template file)
    (pipe
     file read-lines
     head (else "")
     into))

  (declare read-insertion-mapping (string -> (trie (tuple char char) char)))
  (define (read-insertion-mapping file)
    (pipe file read-lines (drop 1)
     (fold (fn* (line)
             (let* (((cons pair (cons insertion (nil))) line (split-string " -> ") (filter (/= ""))))
               (trie-set (pipe pair into
                               (fn* ((cons left (cons right (nil))))
                                 (tuple left right)))
                         (pipe insertion into head (else #\0)))))
           empty-trie)))

  (declare polymerize ((list char) -> (trie (tuple char char) char) -> (trie char integer)))
  (define (polymerize chars reactions)
    "Given a starting polymer CHARS and an REACTIONS, return the
frequencies of each letter after 40 polymerizations."
    (loop* ((iterations-left 40)
            (polymer (pair-frequencies chars))
            (element-count (frequencies chars)))
      (if (== 0 iterations-left)
          element-count
          (let* ((new-triples polymer
                               (map-trie tuple)
                               (fold
                                (fn* ((tuple (tuple left right) num) triples)
                                  reactions (trie-ref (tuple left right))
                                  (*match
                                    ((none) triples) ;not in insertion mapping, a new character (tripple) will NOT be inserted
                                    ((some c) (cons (tuple (tuple3 left c right) num)
                                                    triples))))
                                (make-list))))
            (recur (- iterations-left 1)
                   ;; new polymer
                   (fold (fn* ((tuple (tuple3 left mid right) num) polymer)
                           polymer
                           (trie-replace-by 0 (+ num) (tuple left mid))
                           (trie-replace-by 0 (+ num) (tuple mid right)))
                         (fold (fn* ((tuple (tuple3 left _ right) _))
                                 ;; this pair of characters are no longer next to each other; they will be removed (set to 0)
                                 (trie-set (tuple left right) 0))
                               polymer new-triples)
                         new-triples)
                   ;; new character frequencies
                   (fold (fn* ((tuple (tuple3 _ mid _) num))
                           (trie-replace-by 0 (+ num) mid))
                         element-count new-triples))))))

  (declare rate-polymer ((trie char integer) -> integer))
  (define (rate-polymer frequencies)
    (let* ((most (fn* (op)
                   frequencies
                   (map-trie tuple)
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
