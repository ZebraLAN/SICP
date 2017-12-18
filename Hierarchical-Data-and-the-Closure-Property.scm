;;; Exercise 2.17
(define (last-pair lst)
  (let ((tail (cdr lst)))
   (if (null? tail)
       (list (car lst))
       (last-pair tail))))

;;; Exercise 2.18
(define (reverse lst)
  (define (reverse-iter lst acc)
    (if (null? lst)
        acc
        (reverse-iter (cdr lst)
                      (cons (car lst) acc))))
  (reverse-iter lst ()))

;;; Exercise 2.19
(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

;;; Exercise 2.20
(define (same-parity first . rest)
  (define same? (if (odd? first) odd? even?))
  (define (same-parity-rec rest)
    (if (null? rest)
        ()
        (let ((second (car rest)))
         (if (same? second)
             (cons second (same-parity-rec (cdr rest)))
             (same-parity-rec (cdr rest))))))
  (cons first (same-parity-rec rest)))

;;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;;; Exercise 2.23
(define (for-each proc items)
  (map proc items)
  #t)

;;; Exercise 2.25
(car (cdaddr '(1 3 (5 7) 9)))

(caar '((7)))

(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

;;; Exercise 2.26
'(1 2 3 4 5 6)

'((1 2 3) 4 5 6)

'((1 2 3) (4 5 6))

;;; Exercise 2.27
(define (deep-reverse tree)
  (define (deep-reverse-iter tree acc)
    (if (null? tree)
        acc
        (let ((node (car tree)))
         (deep-reverse-iter (cdr tree)
                            (cons (if (list? node)
                                      (deep-reverse-iter node nil)
                                      node)
                                  acc)))))
  (deep-reverse-iter tree nil))

(define (fringe tree)
  (if (null? tree)
      nil
      (let ((node (car tree)))
       (append (if (list? node)
                   (fringe node)
                   (list node))
               (fringe (cdr tree))))))

;;; Exercise 2.29
(define left-branch car)

(define right-branch cadr)

(define branch-length car)

(define branch-structure cadr)

(define mobile? pair?)

(define (total-weight mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (+ (structure-weight (branch-structure left))
       (structure-weight (branch-structure right)))))

(define (structure-weight structure)
  (if (mobile? structure)
      (total-weight structure)
      structure))

(define (balanced? mobile)
  (define (branch-torque branch)
    (* (branch-length branch)
       (structure-weight (branch-structure branch))))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (= (branch-torque left) (branch-torque right))))

;;; Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square tree)))
       tree))

;;; Exercise 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

;;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (subset) (cons (car s) subset))
                     rest)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;; Exercise 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (_ len) (+ 1 len)) 0 sequence))

;;; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;; Exercise 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (sub-t)
                         (if (pair? sub-t) (count-leaves sub-t) 1))
                       t)))

;;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;;; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
   (map (lambda (v) (matrix-*-vector m v)) cols)))

;;; Exercise 2.39
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ 1 low) high))))

;;; Exercise 2.40
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (ordered-sum-triples n s)
  (filter (lambda (triple) (= (accumulate + 0 triple) s))
          (flatmap (lambda (i)
                     (map (lambda (pair) (cons i pair))
                          (unique-pairs (- i 1))))
                   (enumerate-interval 1 n))))

;;; Exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-positions new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board ())

(define (adjoin-positions r k positions)
  (cons (cons r k) positions))

(define (safe? k positions)
  (let ((position (car (filter (lambda (position)
                                 (= (cdr position) k))
                               positions))))
    (accumulate (lambda (x y) (and x y))
                #t
                (map (lambda (other-position)
                       (not (threatens? other-position position)))
                     (filter (lambda (position)
                               (not (= k (cdr position))))
                             positions)))))

(define (threatens? p1 p2)
  (or (= (car p1) (car p2))
      (= (abs (- (car p1) (car p2)))
         (abs (- (cdr p1) (cdr p2))))))

;;; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
       (below painter (beside smaller smaller)))))

;;; Exercise 2.45
(define (split op1 op2)
  (define (split-op painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-op painter (- n 1))))
         (op1 painter (op2 smaller smaller)))))
  split-op)

;;; Exercise 2.46
(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect factor vect)
  (make-vect (* factor (xcor-vect vect))
             (* factor (ycor-vect vect))))

;;; Exercise 2.47

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (edge2-frame frame)
  (cddr frame))

;;; Exercise 2.48

(define (make-segment . cors)
  (cons (make-vect (car cors) (cadr cors))
        (make-vect (caddr cors) (cadddr cors))))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;;; Exercise 2.49

(define outline
  (segments->painter
    (list
      (make-segment 0 0 1 0)
      (make-segment 0 0 0 1)
      (make-segment 1 0 1 1)
      (make-segment 0 1 1 1))))

(define x
  (segments->painter
    (list
      (make-segment 0 0 1 1)
      (make-segment 0 1 1 0))))

(define diamond
  (segments->painter
    (list
      (make-segment 0   0.5 0.5 0  )
      (make-segment 0   0.5 0.5 1  )
      (make-segment 0.5 0   1   0.5)
      (make-segment 0.5 1   1   0.5))))

;;; Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
   (let ((paint-down
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 0.5 0.0)
                              split-point))
         (paint-up
           (transform-painter painter2
                              split-point
                              (make-vect 0.5 0.5)
                              (make-vect 0.0 1.0))))
     (lambda (frame)
       (paint-down frame)
       (paint-up frame)))))
