(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;;; Exercise 2.53
'(a b c)

'((george))

'(y1 y2)

#f

#f

#t

;;; Exercise 2.54
(define (equal? x y)
  (or (and (not (pair? x))
           (not (pair? y))
           (eq? x y))
      (and (pair? x)
           (pair? y)
           (equal? (car x) (car y))
           (equal? (cdr x) (cdr y)))))

;;; Exercise 2.55
;;;     ''abracadabra is first transformed into (quote (quote abracadabra)),
;;;     which evaluates to (quote abracadadra), whose car is quote."

;;;

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;;; Exercise 2.56

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (let ((exponent (exponent exp))
               (base (base exp)))
           (make-product
             (make-product exponent
                           (make-exponentiation base
                                                (make-sum exponent -1)))
             (deriv base var))))
        (else
          (error "unknown expression type -- DERIV" exp))))

;;; Exercise 2.57
(define (augend s)
  (let ((augends (cddr s)))
   (if (= (length augends) 1)
       (car augends)
       (cons '+ augends))))

(define (multiplicand m)
  (let ((multiplicands (cddr m)))
   (if (= (length multiplicands) 1)
       (car multiplicands)
       (cons '* multiplicands))))

;;; Exercise 2.58 a

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

;;; Exercise 2.58 b

(define (sum? x) (memq '+ x))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else ((if (or (number? a1) (variable? a1)) cons append)
               a1
               ((if (or (number? a2) (variable? a2)) list cons) '+ a2)))))

(define (addend s)
  (define (rec rest)
    (let ((exp (car rest)))
     (if (eq? exp '+)
         ()
         (cons exp (rec (cdr rest))))))
  (let ((form (rec s)))
   (if (= (length form) 1)
       (car form)
       form)))

(define (augend s)
  (define (rec rest)
    (let ((exp (car rest)))
     (if (eq? exp '+)
         (cdr rest)
         (rec (cdr rest)))))
  (let ((form (rec s)))
   (if (= (length form) 1)
       (car form)
       form)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else ((if (or (number? m1) (variable? m1) (sum? m1))
                   cons 
                   append)
               m1
               ((if (or (number? m2) (variable? m2) (sum? m2))
                    list
                    cons)
                '*
                m2)))))


(define (multiplicand s)
  (let ((exp (cddr s)))
   (if (= (length exp) 1)
       (car exp)
       exp)))

;;; Sets as unordered lists

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (element-of-set? x set)
  (and (not (null? set))
       (or (equal? x (car set))
           (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;; Exercise 2.59
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (let ((item (car set1)))
       (if (element-of-set? item set2)
           (union-set (cdr set1) set2)
           M
           (cons item (union-set (cdr set1) set2))))))

;;; Exercise 2.60
(define (adjoin-set x set) (cons x set))

(define (union-set set1 set2) (append set1 set2))

;;;
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
       (cond ((= x1 x2)
              (cons x1
                    (intersection-set (cdr set1)
                                      (cdr set2))))
             ((< x1 x2)
              (intersection-set (cdr set1) set2))
             ((< x2 x1)
              (intersection-set set1 (cdr set2)))))))

;;; Exercise 2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr x))))))

;;; Exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
               (cond ((= x1 x2)
                      (cons x1
                            (union-set (cdr set1)
                                       (cdr set2))))
                     ((< x1 x2)
                      (cons x1
                            (union-set (cdr set1) set2)))
                     ((< x2 x1)
                      (cons x2
                            (union-set set1 (cdr set2)))))))))

;;; Sets as binary trees
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;;; Exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;;; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
       (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

"For n = 0, @partial-tree@ simply leave @elts@ as is and return ('() . elts)."
"For n > 0, @partial-tree@ first call itself to make a sub-tree as "
"@left-branch@. Then it take the @car@ of remaining elts as @entry@ and call "
"itself again to make a @right-branch@. Finally, it call @make-tree@ to "
"construct a tree from @left-branch@, @entry@ and @right-branch@, and return "
"the tree with remaining elts."

;;; Exercise 2.65
(define (union-set set1 set2)
  (define (union-list list1 list2)
    (if (or (null? list1) (null? list2))
        '()
        (let ((x1 (car list1)) (x2 (car list2)))
         (cond ((= x1 x2)
                (cons x1
                      (union-list (cdr list1) (cdr list2))))
               ((< x1 x2)
                (cons x1
                      (union-list (cdr list1) list2)))
               ((< x2 x1)
                (cons x2
                      (union-list list1 (cdr list2))))))))
  (list->tree
    (union-list (tree->list-1 set1)
                (tree->list-1 set2))))

(define (intersection-set set1 set2)
  (define (intersection-list list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else (let ((x1 (car list1)) (x2 (car list2)))
                 (cond ((= x1 x2)
                        (cons x1
                              (intersection-list (cdr list1) (cdr lilst2))))
                       ((< x1 x2)
                        (intersection-list (cdr list1) llist2))
                       ((> x1 x2)
                        (intersection-list list1 (cdr llist2))))))))
  (list->tree
    (intersection-list
      (tree->list-1 set1)
      (tree->list-1 set2))))

;;; Sets and information retrieval
(define (lookup given-key set-of-record)
  (cond ((null? set-of-record) false)
        ((equal? given-key (key (entry set-of-record)))
         (entry set-of-record))
        ((< given-key (key (entry set-of-record)))
         (lookup given-key (left-branch set-of-record)))
        ((> given-key (key (entry set-of-record)))
         (lookup given-key (right-branch set-of-record)))))

;;; 2.3.4 Example: Huffman Encoding Trees

;;; Representing Huffman trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;;; The decoding procedure
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;;; Sets of weighted elements

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
       (adjoin-set (make-leaf (car pair)
                              (cadr pair))
                   (make-leaf-set (cdr pairs))))))

;;; Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;; Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (let ((left (left-branch tree)))
    (cond ((and (leaf? left) (eq? symbol (symbol-leaf left)))
           (list 0))
          ((memq symbol (symbols left))
           (cons 0 (encode-symbol symbol left)))
          (else
            (let ((right (right-branch tree)))
             (cond ((and (leaf? right) (eq? symbol (symbol-leaf right)))
                     (list 1))
                    ((memq symbol (symbols right))
                     (cons 1 (encode-symbol symbol right)))
                    (else
                      (error "symbol not found -- ENCODE-SYMBOL" symbol))))))))

;;; Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (let ((elt (car set))
        (rest (cdr set)))
   (if (null? rest)
       elt
       (sucessive-merge
         (adjoin-set (make-code-tree elt (car rest))
                     (cdr rest))))))

;;; Exercise 2.70
(define rock-song-tree
  (generate-huffman-tree
    '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))

(define rock-song-message
  (append
    '(Get a job)
    '(Sha na na na na na na na na)
    '(Get a job)
    '(Sha na na na na na na na na)
    '(Wah yip yip yip yip yip yip yip yip yip)
    '(Sha boom)))

(define rock-song-bits
  (encode rock-song-message rock-song-tree))

;;; Exercise 2.71
"bits needed for most frequent symbol:  1"
"bits needed for least frequent symbol: 4"

;;; Exercise 2.72
