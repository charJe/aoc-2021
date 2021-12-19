(cl:in-package #:charje.advent-of-code)

(coalton-toplevel
  (define-type display
    (display (list string)
             (list string)))

  (define (display-actual p)
    (match p
      ((display _ a) a)))

  (define (display-pattern p)
    (match p
      ((display a _) a)))

  (define input
    (let* ((process (compose (filter (/= "")) (split #\space))))
      "input/08-practice"
      read-lines
      (map (split #\|))
      (map (fn (line)
             (match line
               ((nil) (display nil nil))
               ((cons left (cons right (nil)))
                (display (process left)
                         (process right))))))))

  (define code-mapping
    (make-hash
     ("abcefg" 0)
     ("cf" 1)
     ("acdeg" 2)
     ("acdfg" 3)
     ("bcdf" 4)
     ("abdfg" 5)
     ("abdefg" 6)
     ("acf" 7)
     ("abcdefg" 8)
     ("abcdfg" 9)))

  (define unique-mapping
    (make-hash
     (2 (make-list #\c #\f))
     (4 (make-list #\b #\c #\d #\f))
     (3 (make-list #\a #\c #\f))))

  (define default-mapping
    (make-hash
     (#\a (make-list #\a #\b #\c #\d #\e #\f #\g))
     (#\b (make-list #\a #\b #\c #\d #\e #\f #\g))
     (#\c (make-list #\a #\b #\c #\d #\e #\f #\g))
     (#\d (make-list #\a #\b #\c #\d #\e #\f #\g))
     (#\e (make-list #\a #\b #\c #\d #\e #\f #\g))
     (#\f (make-list #\a #\b #\c #\d #\e #\f #\g))
     (#\g (make-list #\a #\b #\c #\d #\e #\f #\g))))

  (define letters (pipe default-mapping hash-keys sort))

  (define (eliminate-single removals c mapping)
    (hash-replace-by
     nil (filter (compose not (flip member removals)))
     c mapping))

  (define (keep-single keeps c mapping)
    (hash-replace-by
     nil
     (filter (flip member keeps))
     c mapping))

  (define (eliminate-unique code mapping)
    (let* ((possibles code string-length
                       (flip hash-ref unique-mapping)))
      (match* possibles
        ((none) mapping)
        ((some possibles)
         letters
         (filter (compose not (flip member (into code))))
         (fold (eliminate-single possibles) mapping)
         (flip (fold (keep-single possibles)) (into code))))))

  (define (eliminate-others-keep-self self c mapping)
    (pipe
     letters (filter (/= self))
     (fold (eliminate-single (make-list c)) mapping)
     (keep-single (make-list c) self)))

  (declare decode ((hash char (list char)) * string * integer))
  (define (decode mapping code)
    (pipe
     code into
     (map (fn* (char)
            mapping (hash-ref char) (else nil)
            head (else #\0)))
     sort into
     (flip hash-ref code-mapping) (else 0)))

  (define (possible-final-mappings mapping)
    (pipe
     letters
     (fold
      (fn* (letter mappings)
        mappings
        (map (fn* (m)
               (hash-ref letter m) (else nil)
               (map (fn* (c)
                      (eliminate-others-keep-self letter c m)))))
        concat
        (append mappings))
      (make-list mapping))))

  (define (valid-mapping pattern mapping)
    (pipe
     pattern
     (map (decode mapping))
     sum
     (== 45)))

  (define (decode-display display)
    (let* ((pattern display display-pattern)
            (mapping pattern
                     (fold eliminate-unique default-mapping)
                     possible-final-mappings
                     (any-value (valid-mapping pattern))
                     (else empty-hash)))
      display display-actual
      (map (decode mapping))
      reverse
      (map into)
      (reduce concat-string) (else "")
      parse-int (else 0)))

  (define (decode-displays displays)
    (pipe displays
          (map decode-display)
          sum))
  )
