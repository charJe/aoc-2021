(cl:in-package #:charje.advent-of-code)


(coalton-toplevel
  (define (read-lines filename)
    (lisp (list string) (filename)
       (str:split #\newline
                  (uiop:read-file-string filename)
                  :omit-nulls cl:t)))

  (define (split-string d s)
    (lisp (list string) (d s)
       (str:split d s :omit-nulls cl:t)))

  (define (fromsome-parse-int x)
    (fromsome (concat-string x " is not an integer")
              (parse-int x)))

  (declare replace-by ((:a -> :a) -> integer -> (list :a) -> (list :a)))
  (define (replace-by f i l)
    (let ((%replace-by
            (fn (i start end)
              (let ((h (fromsome "no head" (head end))))
                (if (== 0 i)
                    (append (reverse start)
                            (cons (f h) (drop 1 end)))
                    (%replace-by
                     (- i 1)
                     (cons h start)
                     (drop 1 end)))))))
      (%replace-by i nil l)))

  (define-instance (Functor List)
    (define (map f l)
      (lisp (list :a) (f l)
        (cl:map 'cl:list f l))))

  (define (append xs ys)
    (lisp (list :a) (xs ys)
      (cl:append xs ys)))

  (define (else default o)
    (match o
      ((some o) o)
      ((none) default)))

  ;;; funds bindings
  ;; hash-table
  (define-type (hash :k :v))
  (define empty-hash
    (lisp lisp-object ()
      (funds:make-hash)))

  (declare hash-set (:k -> :v -> lisp-object -> lisp-object))
  (define (hash-set key value hash)
    (lisp lisp-object (key value hash)
      (funds:hash-set hash key value)))

  (declare hash-ref (:k -> lisp-object -> (optional :v)))
  (define (hash-ref key hash)
    (match (lisp (tuple :v boolean) (key hash)
             (cl:multiple-value-bind (value present)
                 (funds:hash-ref hash key)
               (cl:eval `(coalton (tuple ,value ,present)))))
      ((tuple v (true)) (some v))
      ((tuple _ (false)) none)))

  (declare hash-keys (lisp-object -> (list :v)))
  (define (hash-keys hash)
    (lisp (list :v) (hash)
      (funds:hash-keys hash)))
  )
