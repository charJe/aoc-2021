(cl:in-package #:charje.advent-of-code)

(cl:defmacro my-pipe (cl:&body forms)
  (cl:if (cl:= (cl:length forms) 1)
         (cl:first forms)
         `(pipe ,@forms)))

(cl:defmacro define* (var-or-fun cl:&body body)
  `(define ,var-or-fun
     (my-pipe ,@body)))

(cl:defmacro match* (expr cl:&body patterns)
  `(match ,expr
     ,@(cl:mapcar
      (cl:lambda (pattern)
        (cl:cons (cl:car pattern)
                 `((my-pipe ,@(cl:cdr pattern)))))
      patterns)))

(cl:defmacro let* (bindings cl:&body body)
  (cl:let* ((sym-args (cl:loop :for arg :in (cl:mapcar 'cl:first bindings)
                        :if (cl:listp arg)
                          :collect (cl:gensym)
                         :else
                           :collect arg))
            (match-pairs (cl:loop :for arg :in (cl:mapcar 'cl:first bindings)
                            :for sym :in sym-args
                            :if (cl:listp arg)
                              :collect (cl:cons arg sym)))
            (matches (cl:reduce
                      (cl:lambda (body match-pair)
                        (cl:destructuring-bind (arg . sym)
                            match-pair
                          `(match ,sym
                             (,arg ,body))))
                      match-pairs
                      :initial-value
                      `(my-pipe
                         ,@body))))
      `(let ,(cl:mapcar 'cl:cons
              sym-args
              (cl:mapcar (cl:lambda (binding)
                           `((my-pipe ,@(cl:rest binding))))
                         bindings))
     ,matches)))

(cl:defmacro fn* (args cl:&body body)
  (cl:let* ((sym-args (cl:loop :for arg :in args
                        :if (cl:listp arg)
                          :collect (cl:gensym)
                         :else
                           :collect arg))
            (match-pairs (cl:loop :for arg :in args
                            :for sym :in sym-args
                            :if (cl:listp arg)
                              :collect (cl:cons arg sym)))
            (matches (cl:reduce
                      (cl:lambda (body match-pair)
                        (cl:destructuring-bind (arg . sym)
                            match-pair
                          `(match ,sym
                             (,arg ,body))))
                      match-pairs
                      :initial-value
                      `(my-pipe
                         ,@body))))
    `(fn ,sym-args ,matches)))

(cl:defmacro loop* (bindings cl:&body body)
  (cl:let ((name 'recur))
    (cl:when (cl:symbolp bindings)
      (cl:setq name bindings
               bindings (cl:first body)
               body (cl:rest body)))
  `(let ((,name (fn* ,(cl:mapcar 'cl:first bindings)
                  ,@body)))
     (,name ,@(cl:mapcar (cl:lambda (binding)
                        (cons 'my-pipe (cl:rest binding)))
                      bindings)))))

(coalton-toplevel
  (define (read-lines filename)
    (lisp (list string) (filename)
       (str:split #\newline
                  (uiop:read-file-string filename)
                  :omit-nulls cl:t)))

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

    (declare scan
           ((:a -> :b -> :b)
            -> :b
            -> (list :a)
            -> (list :b)))
  (define (scan f y l)
    (let ((%scan
            (fn (as l)
              (match l
                ((nil) (reverse as))
                ((cons h t)
                 (match as
                   ((cons a _)
                    (%scan (cons (f h a) as) t))))))))
      (%scan (cons y nil) l)))

  (declare scany
           ((:a -> :a -> :a)
            -> (list :a)
            -> (list :a)))
  (define (scany f l)
    (match l
      ((cons h t)
       (scan f h t))
      ((nil) nil)))

  (define (reduce f l)
    (match l
      ((nil) none)
      ((cons single (nil)) (some single))
      ((cons x xs) (some (fold f x xs)))))

  (define (map-index f xs)
    (zipwith f xs (range 0 (length xs))))

  (define (else default o)
    (match o
      ((some o) o)
      ((none) default)))

  (define (any-value p xs)
    (match xs
      ((nil) none)
      ((cons x xs)
       (if (p x)
           (some x)
           (any-value p xs)))))

  ;;; funds bindings
  ;; hash-table
  (define-type (hash :k :v)
    (hash lisp-object))

  (define empty-hash
    (hash (lisp lisp-object ()
            (funds:make-hash :test #'cl:equal))))

  (declare hash-set (:k -> :v -> (hash :k :v) -> (hash :k :v)))
  (define (hash-set k v h)
    (match h
      ((hash h)
       (hash (lisp lisp-object (k v h)
               (funds:hash-set h k v))))))

  (declare hash-remove (:k -> (hash :k :v) -> (hash :k :v)))
  (define (hash-remove k h)
    (match h
      ((hash h)
       (hash (lisp lisp-object (k h)
               (funds:hash-remove k h))))))

  (declare hash-ref (:k -> (hash :k :v) -> (optional :v)))
  (define (hash-ref k h)
    (match h
      ((hash h)
       (match (lisp (tuple :v boolean) (k h)
                (cl:multiple-value-bind (value present)
                    (funds:hash-ref h k)
                  (tuple value present)))
         ((tuple v (true)) (some v))
         ((tuple _ (false)) none)))))

  (declare hash-keys ((hash :k :v) -> (list :k)))
  (define (hash-keys h)
    (match h
      ((hash h)
       (lisp (list :k) (h)
         (funds:hash-keys h)))))

  (declare map-hash ((:k -> :v -> :a) -> (hash :k :v) -> (list :a)))
  (define (map-hash f h)
    (match h
      ((hash h)
       (lisp (list :a) (f h)
         (cl:let ((kv (funds:hash-as-alist h)))
           (zipwith f
                    (cl:mapcar 'cl:car kv)
                    (cl:mapcar 'cl:cdr kv)))))))

  (define (hash-size h)
    (match h
      ((hash h)
       (lisp integer (h)
         (funds:hash-size h)))))

  (define (hash-replace-by default-value f k h)
    (pipe
     h
     (hash-ref k) (else default-value)
     f
     (flip (hash-set k) h)))
  )

(cl:defmacro make-hash (cl:&rest key-values)
  `(pipe
    empty-hash
    ,@(cl:mapcar
       (cl:lambda (key-value)
         (cl:cons 'hash-set key-value))
       key-values)))
