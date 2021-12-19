(cl:in-package #:charje.advent-of-code)

(coalton-toplevel
  (define-type vertex
    start
    (small string)
    (large string)
    end)

  (define-instance (eq vertex)
    (define (== v1 v2)
      (match v1
        ((start)
         (match v2
           ((start) true)
           (_ false)))
        ((end)
         (match v2
           ((end) true)
           (_ false)))
        ((small v1-name)
         (match v2
           ((small v2-name)
            (== v1-name v2-name))
           (_ false)))
        ((large v1-name)
         (match v2
           ((large v2-name)
            (== v1-name v2-name))
           (_ false))))))

  (define (non-returnable v)
    (match v
      ((start) true)
      ((small _) true)
      (_ false)))

  (define (make-vertex s)
    (cond
      ((== s "end") end)
      ((== s "start") start)
      ((all (> #\Z) (into s)) (large s))
      (true (small s))))

  (declare input (string -> (hash vertex (list vertex))))
  (define (input file)
    (pipe
     file
     read-lines
     (map (fn (line)
            (match (split #\- line)
              ((cons a (cons b (nil)))
               (make-list (make-list (make-vertex a) (make-vertex b))
                          (make-list (make-vertex b) (make-vertex a)))))))
     concat
     (fold (fn* ((cons v vs) adjacencies)
             (let* ((existing-vs adjacencies (hash-ref v) (else nil)))
               adjacencies (hash-set v (remove-duplicates (append vs existing-vs)))))
           empty-hash)
     (hash-remove end)))

  (define (count-paths adjacencies)
    (loop* ((visited empty-hash)
            (twice false)
            (spot start))
      (if (== end spot)
          1
          (pipe
           adjacencies (hash-ref spot) (else nil)
           (map (fn* (option)
                  (let* ((already-visited visited (hash-contains option))
                         (new-visited
                           (if (non-returnable option)
                               (hash-add option visited)
                               visited)))
                    (cond
                      ((or (and already-visited twice)
                           (== start option))
                       0)
                      ((and already-visited (not twice))
                       (recur new-visited true option))
                      (true
                       (recur new-visited twice option))))))
           sum))))

  )
