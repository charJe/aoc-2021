(cl:in-package #:charje.advent-of-code)

(cl:defmacro my-pipe (cl:&body forms)
  (cl:if (cl:= (cl:length forms) 1)
         (cl:first forms)
         `(pipe ,@forms)))

(cl:defmacro fp (cl:&body forms)
  `(fn* (x) x ,@forms))

(cl:defmacro define* (var-or-fun cl:&body body)
  `(define ,var-or-fun
     (my-pipe ,@body)))


(cl:defmacro cond* (cl:&body clauses)
  `(cond
     ,@(cl:mapcar
        (cl:lambda (clause)
          (cl:cons (cl:car clause)
                   `((my-pipe ,@(cl:cdr clause)))))
        clauses)))

(cl:defmacro match* (expr cl:&body patterns)
  `(match ,expr
     ,@(cl:mapcar
        (cl:lambda (pattern)
          (cl:cons (cl:car pattern)
                   `((my-pipe ,@(cl:cdr pattern)))))
        patterns)))

(cl:defmacro *match (cl:&body patterns)
  (cl:let ((x (cl:gensym)))
    `(fn (,x)
       (match* ,x
         ,@patterns))))

(cl:defun make-matches (args body)
  (cl:let* ((sym-args (cl:loop :for arg :in args
                         :if (cl:listp arg)
                           :collect (cl:gensym)
                         :else
                           :collect arg))
            (match-pairs (cl:loop :for arg :in args
                            :for sym :in sym-args
                            :if (cl:listp arg)
                              :collect (cl:cons arg sym))))
    (cl:values
     sym-args
     (cl:reduce
      (cl:lambda (body match-pair)
        (cl:destructuring-bind (arg . sym)
            match-pair
          `(match ,sym
             (,arg ,body))))
      match-pairs
      :initial-value
      `(my-pipe
         ,@body)))))

(cl:defmacro let* (bindings cl:&body body)
  (cl:multiple-value-bind (sym-args matches)
      (make-matches (cl:mapcar 'cl:first bindings) body)
    `(let ,(cl:mapcar 'cl:cons
            sym-args
            (cl:mapcar (cl:lambda (binding)
                         `((my-pipe ,@(cl:rest binding))))
                       bindings))
       ,matches)))

(cl:defmacro fn* (args cl:&body body)
  (cl:multiple-value-bind (sym-args matches) (make-matches args body)
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
  (define (print x)
    (progn
      (lisp integer (x)
        (cl:progn
          (cl:princ x)
          (cl:princ #\newline)
          (cl:force-output)
          0))
      x))

  (declare read-lines (string -> (list string)))
  (define (read-lines filename)
    (lisp (list string) (filename)
      (str:split #\newline
                 (uiop:read-file-string filename)
                 :omit-nulls cl:t)))

  (define (string-starts-with/3 ignore-case start s)
    (lisp boolean (ignore-case start s)
      (str:starts-with-p start s :ignore-case ignore-case)))

  (define string-starts-with/2
    (string-starts-with/3 false))

  (declare split-string (string -> string -> (list string)))
  (define (split-string sep s)
    (lisp (list string) (sep s)
      (str:split sep s :omit-nulls nil)))

  (declare join (string -> (list string) -> string))
  (define (join sep strings)
    (lisp string (sep strings)
      (str:join sep strings)))

  (define (fromsome-parse-int x)
    (fromsome (concat-string x " is not an integer")
              (parse-int x)))

  (define (tuple-0 t)
    (match t
      ((tuple t0 _) t0)))

  (define (tuple-1 t)
    (match t
      ((tuple _ t1) t1)))

  (declare replace-by ((:a -> :a) -> integer -> (list :a) -> (list :a)))
  (define (replace-by f i l)
    (let ((%replace-by
            (fn (i start end)
              (let ((h (fromsome "replace-by: no head" (head end))))
                (if (== 0 i)
                    (append (reverse start)
                            (cons (f h) (drop 1 end)))
                    (%replace-by
                     (- i 1)
                     (cons h start)
                     (drop 1 end)))))))
      (%replace-by i nil l)))

  (define (matrix-replace-by f r c matrix)
    (replace-by (replace-by f c)
                r matrix))

  (define-instance (functor list)
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

  (define (zipwith f xs ys)
    (loop* ((xs xs) (ys ys)
            (zipped nil))
    (match (tuple xs ys)
      ((tuple (cons x xs)
              (cons y ys))
       (recur xs ys (cons (f x y) zipped)))
      (_ zipped))))
  
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

  (define (frequencies xs)
    (fold (trie-replace-by 0 (+ 1)) empty-trie xs))
  
  ;;; funds bindings
  ;; queue
  (define-type (priority-queue :a)
    (pqueue lisp-object))

  (define empty-pqueue
    (pqueue
     (lisp lisp-object ()
       (funds:make-heap ))))

  (declare enpqueue
           (:a -> integer -> (priority-queue :a)
               -> (priority-queue :a)))
  (define (enpqueue e p q)
    (match q
      ((pqueue q)
       (pqueue
        (lisp lisp-object (e p q)
          (funds:heap-insert q e p))))))

  (declare pqueue-top ((priority-queue :a) -> (optional :a)))
  (define (pqueue-top q)
    (if (pqueue-empty q)
        none
        (match q
          ((pqueue q)
           (some
            (lisp :a (q)
              (funds:heap-first q)))))))

  (declare depqueue
           ((priority-queue :a)
            -> (priority-queue :a)))
  (define (depqueue q)
    (match q
      ((pqueue q)
       (pqueue
        (lisp lisp-object (q)
          (funds:heap-remove q))))))

  (define (pqueue-empty q)
    (match q
      ((pqueue q)
        (lisp boolean (q)
          (funds:heap-empty-p q)))))

  (define-instance (into (priority-queue :a) (list :a))
    (define (into q)
      (match q
        ((pqueue q)
         (lisp (list :a) (q)
           (funds:queue-as-list q))))))
  
  ;; vector
  (define-type (vector :a)
    (vector lisp-object))

  (define (make-sized-vector size initial-element)
    (vector (lisp lisp-object (size initial-element)
              (funds:make-vector size :initial-element initial-element))))

  (define-instance (Into (vector :a) (list :a))
    (define (into vec)
      (match vec
        ((vector v)
         (lisp (list :a) (v)
           (funds:vector-as-list v))))))

  (define-instance (Into (list :a) (vector :a))
    (define (into xs)
      (vector
       (lisp lisp-object (xs)
         (funds:vector-from-list xs)))))

  (define-instance (functor vector)
    (define (map f vec)
      (the (vector :a)
           (into (map f (the (list :a) (into vec)))))))

  (define (vector-length vec)
    (match vec
      ((vector v)
       (lisp integer (v)
         (funds:vector-size v)))))

  (define (vector-index vec index)
    (match vec
      ((vector v)
       (match (lisp (tuple :v boolean) (v index)
                (cl:multiple-value-bind (value present)
                    (funds:vector-ref v index)
                  (tuple value present)))
         ((tuple v (true)) (some v))
         ((tuple _ (false)) none)))))

  (define (vector-set index item vec)
    (match vec
      ((vector v)
       (vector (lisp lisp-object (v index item)
                 (funds:vector-set v index item))))))

  (declare vector-replace-by
           (:a -> (:a -> :a) -> integer -> (vector :a)
               -> (vector :a)))
  (define (vector-replace-by default f index vec)
    (vector-set
     index
     (pipe (vector-index vec index) (else default) f)
     vec))

  ;; hash-table
  (define-type (trie :k :v)
    (trie lisp-object))

  (declare empty-trie (trie :k :v))
  (define empty-trie
    (trie (lisp lisp-object ()
            (funds:make-hash
             :hash (cl:lambda (a)
                     (cl:sxhash (cl:prin1-to-string a)))
             :test (cl:lambda (a b)
                     (cl:eval `(coalton (== ,(cl:prin1-to-string a)
                                            ,(cl:prin1-to-string b)))))))))
  
  (declare trie-set (:k -> :v -> (trie :k :v) -> (trie :k :v)))
  (define (trie-set k v h)
    (match h
      ((trie h)
       (trie (lisp lisp-object (k v h)
               (funds:hash-set h k v))))))

  (declare trie-add (:k -> (trie :k :v) -> (trie :k :v)))
  (define (trie-add k h)
    (match h
      ((trie h)
       (trie (lisp lisp-object (k h)
               (funds:hash-set h k))))))

  (declare trie-remove (:k -> (trie :k :v) -> (trie :k :v)))
  (define (trie-remove k h)
    (match h
      ((trie h)
       (trie (lisp lisp-object (k h)
               (funds:hash-remove h k))))))

  (declare trie-ref (:k -> (trie :k :v) -> (optional :v)))
  (define (trie-ref k h)
    (match h
      ((trie h)
       (match (lisp (tuple :v boolean) (k h)
                (cl:multiple-value-bind (value present)
                    (funds:hash-ref h k)
                  (tuple value present)))
         ((tuple v (true)) (some v))
         ((tuple _ (false)) none)))))

  (declare trie-contains (:k -> (trie :k :v) -> boolean))
  (define (trie-contains k h)
    (match h
      ((trie h)
       (lisp boolean (k h)
         (cl:nth-value 1 (funds:hash-ref h k))))))

  (declare trie-keys ((trie :k :v) -> (list :k)))
  (define (trie-keys h)
    (match h
      ((trie h)
       (lisp (list :k) (h)
         (funds:hash-keys h)))))

  (declare map-trie ((:k -> :v -> :a) -> (trie :k :v) -> (list :a)))
  (define (map-trie f h)
    (match h
      ((trie h)
       (lisp (list :a) (f h)
         (cl:let ((kv (funds:hash-as-alist h)))
           (zipwith f
                    (cl:mapcar 'cl:car kv)
                    (cl:mapcar 'cl:cdr kv)))))))

  (define (trie-size h)
    (match h
      ((trie h)
       (lisp integer (h)
         (funds:hash-size h)))))

  (define (trie-replace-by default-value f k h)
    (pipe
     h
     (trie-ref k) (else default-value)
     f
     (flip (trie-set k) h)))
  )

(cl:defmacro make-trie (cl:&rest key-values)
  `(pipe
    empty-trie
    ,@(cl:mapcar
       (cl:lambda (key-value)
         (cl:cons 'trie-set key-value))
       key-values)))

(cl:defmacro make-vector (cl:&rest values)
  `(the (vector :a) (into (make-list ,@values))))
