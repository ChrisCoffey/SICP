(ns sicpExercises.chapter2)
 ;; fun with rational numbers
;; clojure's list operations are a bit different syntactically from Schemes.
;; instead of cons taking two elements, it requires a list as the tail element, so
;; (cons 1 2) is invalid, but (cons 1 (cons 2 nil)) is fine
;; (def a (cons 1 (cons 2 (cons 3 nil)))) creates the list (1 2 3)
;; (first a) returns 1
;; (next a) returns (2 3)

(defn numer [x] (first x))
(defn denom [x] (second x))


(defn print-rat [x]
  (print (numer x))
  (print "/")
  (print (denom x))
  (println ""))



; 2.1
(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (rem a b))))

(defn make-rat [n d]
  (let [g (gcd n d)
        num (/ n g)
        den (/ d g)
        pos (cond
              (and (pos? num) (pos? den)) true
              (and (neg? num) (neg? den)) true
              :else false)
        ]
    (if pos
      (list num den)
      (list (- (Math/abs num)) (Math/abs den))
    )))


;; points on a plane
(defn point [x y] (list x y))
(defn point-x [p] (first p))
(defn point-y [p] (second p))

(defn line-segment [a b] (list a b))
(defn segment-start [line] (first line))
(defn segment-end [line] (second line))

(defn line-length [l]
  (Math/sqrt (+
               (Math/pow (- (segment-end (point-x l)) (segment-start (point-x l))) 2)
               (Math/pow (- (segment-end (point-y l)) (segment-start (point-y l))) 2)
               )))


(defn print-point [p]
  (print "("  (point-x p)  "," (point-y p)  ")")
  (println ""))

; 2.2
;; midpoint segment

(defn midpoint-segment [line]
  (let [xs (point-x (segment-start line))
        ys (point-y (segment-start line))
        xe (point-x (segment-end line))
        ye (point-y (segment-end line))
        xm (/ (+ xs xe) 2)
        ym (/ (+ ys ye) 2)
         ]
    (point xm ym)
    ))

;; note could do this with points as rationals, there's probably a more concise way

; 2.3
;; implement a rectangle and area computations

(defn rectangle [topLeft bottomRight] (list topLeft bottomRight))

(defn rect-left-side [r]
  (let [xl (point-x (first r))
        yl (point-y (second r))]
  (line-segment (first r) (point xl yl))
    ))

(defn rect-bottom-side [r]
  (line-segment (segment-end (rect-left-side r)) (second r))
  )

(defn rect-top-side [r]
  (let [xl (point-x (second r))
        yl (point-y (first r))]
    (line-segment (first r) (point xl yl))
    ))

(defn rect-right-side [r]
  (line-segment (segment-end (rect-top-side r)) (second r)))



(defn area-rect [r]
  (*
    (line-length (rect-left-side r))
    (line-length (rect-bottom-side r))
    ))

(defn perimeter-rect [r]
  (+
    (* (line-length (rect-left-side r)) 2)
    (* (line-length (rect-bottom-side r)) 2)
  ))

; 2.4
;; lambda calculus based impl of pairs
(defn cons2 [x y]
  (fn [m] (m x y)))

(defn car [z]
  (z (fn [p q] p)))

(defn cdr [z]
  (z (fn [p q] q)))

; 2.5
;; Arithmetic pairs. Represent pairs of a b as 2^a3^b
(defn cons3 [a b]
  (* (Math/pow 2 a) (Math/pow 3 b)))

(defn car3 [z]
  (let [f (fn iter [i acc]
        (if (odd? (int acc))
          i
          (iter (inc i) (/ acc 2))))
        ]
    (f 0 z)
    ))

(defn cdr3 [z]
  (if (zero? (rem z 3))
    (inc (cdr3 (/ z 3)))
    0
    ))

; 2.6
;; Church encoding
(defn zero
  (fn [f]
    (fn [x] x)
    ))

(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

(defn church-one (fn [f] (fn [x] (f x))))
(defn church-two (fn [f] (fn [x] (f (f x)))))
(defn church-add (fn [x] (fn [y] (fn [a] (fn [n] (a (y (a x))))))))


;; Intervals
(defn interval [a b] (list a b))

;; interval arithmetic is the sum of the lower bounds and upper bounds
(defn add-interval [x y]
  (interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(defn multiply-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))
        ]
    (interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
    ))

(defn divide-interval [x y]
  (multiply-interval x
    (interval
      (/ 1.0 (upper-bound y))
      (/ 1.0 (lower-bound y))
  )))

; 2.7
;; upper and lower interval bounds
(defn upper-bound [i] (max (first i) (second i)))
(defn lower-bound [i] (min (first i) (second i)))

; 2.8
;; interval subtraction
;; the difference between two intervals should be the differene between their lower and upper bounds
(defn subtract-interval [x y]
  (interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))

; 2.9
;; interval width
;; The interval width is 1/2 the distance between upper & lower bounds
(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; addition is equivalent to a= 2( w(i) + w(i`)); lower-bound(i) + a, upper-bound(i) + a
;; subtraction is equivalent to a= 2( w(i) + w(i`)); lower-bound(i) - w(i`), upper-bound
;; multiply and divide both com

; 2.10
;; error to go through zero
(defn divide-interval [x y]
  (assert (or
            (and (pos? (lower-bound x)) (pos? (upper-bound x)))
            (and (neg? (lower-bound x)) (neg? (upper-bound x)))
            )
    )
  (multiply-interval x
    (interval
      (/ 1.0 (upper-bound y))
      (/ 1.0 (lower-bound y))
  )))

; 2.11
;; efficient multiplication (so much typing)
(defn multiply-interval' [x y]
  (let [ux (pos? (upper-bound x))
        lx (pos? (lower-bound x))
        uy (pos? (upper-bound y))
        ly (pos? (lower-bound y))
         ]
    (cond
      (and ux (not lx) uy ly) ; [+ -], [+ +]
        (interval
          (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y)))
      (and ux lx uy (not ly)) ; [+ +], [+ -]
        (interval
          (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))
      (and (not ux) (not lx) uy ly) ; [- -], [+ +]
        (interval
          (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
      (and (not ux) (not lx) uy (not ly)) ; [- -], [+ -]
        (interval
          (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)))
      (and ux lx (not uy) (not ly)) ; [+ +], [- -]
        (interval
          (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y)))
      (and ux (not lx) (not uy) (not ly)) ; [+ -], [- -]
        (interval
          (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y)))
      (and (not ux) (not lx) (not uy) (not ly)) ; [- -], [- -]
        (interval
          (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))
      (and ux (not lx) uy (not ly)) ; [+ -], [+ -]
        (interval
          (min (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y)))
          (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
      :else; [+ +], [+ +]
        (interval
          (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))
      )
    ))

; 2.12
;; percentage constructor & selector
(defn interval-center-width [c w]
  (interval (- c w) (+ c w)))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn interval-center-pct [c p]
  (let [w (* c p)]
    (interval-center-width c w)
    ))

(defn pct [i]
  (/ (width i) (center i)))

; 2.13
;; aproximate the center tolerance, assuming all numbers are positive
;; with really small percentage thresholds, adding them together gives a rough aproximation of the multiplied tolearance
(def a (interval-center-pct 10 0.001))
(def b (interval-center-pct 10 0.0001))
(pct (multiply-interval a b)) ; should be .0011

; 2.14
;;parallel formulae returns different results based on equivalent algebraic structures
(defn par1 [r1 r2]
          (divide-interval (multiply-interval' r1 r2) (add-interval r1 r2)))

(defn par2 [r1 r2]
  (let [one (interval 1 1)]
    (divide-interval one (add-interval (divide-interval one r1) (divide-interval one r2)))
    ))

(def pctA (pct (divide-interval a a)))
(def pctAB (pct (divide-interval a b)))
;; percentage is a + b, a + a, whereas a + a should likely be a

; 2.15
;; as noted above, because a/a results in a percentage width of 2a, interval divison should avoid any cases where
;; the same variable is used twice. This is a bizzare consequence of the fact that we're actually multiplying x by
;; the reciprocal of y. Basically interval division a/a != 1

; 2.16
;; Due to the Dependency problem, you can't create a universal solution to interval division. This is because if an
;; interval occurs multiple times and is taken independently each time then it is also expanded each time. The best solution
;; is to create equivalent algebraic statements to reach the same goal.
;; https://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem

;2.17
;; define last-pair on a list
(defn last-pair [ls]
  (if (not (next ls))
    (first ls)
    (last-pair (next ls))
    ))

; 2.18
;; define reverse
(defn reverse-list [ls]
  (let [r (fn iter [a b]
            (if (not (next a))
              (cons (first a) b)
              (iter (next a) (cons (first a) b))
              ))]
    (r ls ())
    ))

; 2.19
;; update the coin changing procedure to take a list of coins rather than hardcoded denominations
(defn except-first-denom [ls] (next ls))
(defn first-denom [ls] (first ls))
(defn no-more? [ls] (empty? ls))

(defn coin-change [amount coins]
  (cond
    (zero? amount) 1
    (or (neg? amount) (no-more? coins)) 0
    :else (+
            (coin-change amount (except-first-denom coins))
            (coin-change (- amount (first-denom coins)) coins))
    ))

; ordering doesn't make a difference because we're ultimately going to walk the coin list to exhaustion for each value

; 2.20
;; same parity
(defn parity? [i & is]
  (let [p (even? i)]
    (filter (fn [e] (= (even? e) p)) is )))

; an impl without using the library filter
(defn filter-rec [p ls]
  (let [f (fn iter [a acc]
            (if (empty? a)
              (reverse acc)
              (if (p (first a))
                (iter (next a) (cons (first a) acc))
                (iter (next a) acc))))
        ]
    (f ls ())
    ))

(defn parity?' [i & is]
  (let [p (even? i)]
    (filter-rec (fn [e] (= (even? e) p)) is )))

; 2.21
;;square the lists of numbers
;a
(defn square-list [ls]
  (if (empty? ls)
    ()
    (cons (* (first ls) (first ls)) (square-list (next ls)))
    ))

(defn square-list [ls]
  (map (fn [x] (* x x)) ls)
  )

; 2.22
;; because you walk from list a -> b, b is constructed in reverse since the first element of a is cons'd to (), 2nd element
;; to 1st, etc... Basicaly, the list is constructed in reverse. No matter how the cons is re-ordered elements are still
;; walked in the same order. To avoid this you need to reverse the list at the end

; 2.23
;; implement for-each, a left to right function appliation that throws away the results

(defn for-each' [ls f]
  (let [r (fn iter [l]
        (if (empty? l)
          true
          (do
            (f (first l))
            (iter (next l))
          )))
        ]
    (r ls)
    ))

; 2.24
(def l (list 1 (list 2 (list 3 4))))

;; box
;; [1] [cdr] -> [2, cdr] -> [3, 4, nil]

;; tree
;;               1
;;              /
;;             2
;;            /\
;;           3  4


; 2.25
;extract 7 from the lists
(def a (list 1 3 (list 5 7) 9))
( second (first (next (next a))))

(def b (list (list 7)))
(first (first b))

(def c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(second (second (second (second (second (second c))))))

; 2.26
;; append cons and such
(def x (list 1 2 3))
(def y (list 4 5 6))

(concat x y) ; (1 2 3 4 5 6)
(cons x y) ; ((1 2 3 ) 4 5 6)
(list x y); ((1 2 3) (4 5 6))

; 2.27
;; deep reverse
(defn deep-reverse-list [ls]
  (let [ g (fn [x] (if (and (list? x) (not (nil? x))) (deep-reverse-list x) x))
         r (fn iter [a b]
            (if (not (next a))
              (cons (g (first a)) b)
              (iter (next a)
                (cons (g (first a)) b))
              ))]
    (r ls ())
    ))

; 2.28
;; create fringe which takes a tree in list form and returns a list of leaf elements in l -> r order
(defn fringe [ls]
    (cond
      (nil? ls) ()
      (not (list? ls)) (list ls)
      :else (concat (fringe (first ls)) (fringe (next ls)))
      )
  )

; 2.29
;; Binary mobile (aka calder)
(defn make-mobile [l r] (list l r))
(defn make-mobile-branch [length structure] (list length structure))

(defn left-branch [mobile] (first mobile))
(defn right-branch [mobile] (second mobile))
(defn branch-length [branch] (first branch))
(defn branch-structure [branch] (second branch))

(defn total-weight [mobile]
  (let [weight (fn [br] (if (number? br) br (total-weight br)))]
  (+  (weight (branch-structure (left-branch mobile))) (weight (branch-structure (right-branch mobile))))
  ))


(defn balanced? [m]
  (let [balanced-branch? (fn [b] (if (number? (branch-structure b)) true (balanced? (branch-structure b))))
        weight (fn [b] (if (number? (branch-structure b)) (branch-structure b) (total-weight (branch-structure b))))
        tourque (fn [b] (* (branch-length b) (weight b)))
        left (left-branch m)
        right (right-branch m)
         ]
      (and
        (balanced-branch? left)
        (balanced-branch? right)
        (= (tourque left) (tourque right))
        )
      )
  )

;; I'd only need to update the accessors. The rest of the logic is hidden away above those.


; 2.30
(defn square-tree [tree]
  (cond
    (nil? tree) nil
    (not (pair? tree)) (* tree tree)
    :else (list (square-tree (first tree)) (square-tree (next tree))))
    )

(defn pair? [ls] (and (list? ls) (= (count ls) 2)))

(defn square-tree' [tree]
  (map (fn [t] (if (pair? t)
         (square-tree' t)
         (* t t)
         )) tree))

(def ts (list (list (list 8 7) 9) (list 2 (list (list 1 8) (list 3 8)))))

; 2.31
;; tree-map

(defn tree-map [f tree]
  (map (fn [t] (if (pair? t) (tree-map f t) (f t))) tree))

(defn square-tree'' [tree] (tree-map (fn [x] (* x x)) tree))

; 2.32
;; powersets. Create the powersets of lists
(defn power-sets [s]
  (if (nil? s)
    (list ())
    (let [rest (power-sets (next s))]
      (concat rest (map #(list (first s) %) rest)))
      ))

;2.33
;; fill in the missing parts of these definitions in terms of accumulate
(defn accumulate [op acc ls]
  (if (empty? ls)
    acc
    (op (first ls) (accumulate op acc (rest ls)))))

(defn map' [p ls]
  (accumulate #((cons (p %1) %2)) () ls))

(defn append [ls rs]
  (accumulate cons rs ls))

(defn length [ls]
  (accumulate inc 0 ls))

; 2.34
;; polynomial evaluation via Horner's rule
;; It works by starting with an, multiply by x, add a(n-1), multiply by x, ... until a0
(defn horner-eval [x coef-seq]
  (accumulate
    (fn [this-coef higher-terms] (+ this-coef (* x higher-terms)))
    0
    coef-seq))


; 2.35
;; redefine count-leaves in terms of accumulate
(defn count-leaves [t]
  (accumulate + 0 (map (fn [x] 1) (fringe t))))

; 2.36
;; accumulate-n, or zip-fold
(defn accumulate-n [op acc seqs]
  (if (empty? (first seqs))
    ()
    (cons (accumulate op acc (map first seqs))
      (accumulate-n op acc (map rest seqs)))))

; 2.37
;; matrix math
;; given matrix:
;;
;; 1 2 3 4
;; 4 5 6 6
;; 6 7 8 9

(def m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map #(dot-product v %) m))

; transpose returns the matrix n, where n(i j) = m(j i)
(defn transpose [mat]
  (accumulate-n cons () mat))

; matrix multiplication is the vector product of each column concatted together
(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map #(matrix-*-vector cols %) m)))

; 2.38
;;fold right and left
(defn fold-left [op acc ls]
  (let [f (fn iter [result tail]
            (if (empty? tail)
              result
              (iter (op result (first tail)) (rest tail))))
        ]
    (f acc ls)
    ))

;; operations should be associative for fold-left & right to produce the same result

; 2.39
;; implement reverse in terms of fold left and right
(defn reverseL [ls]
  (fold-left #(cons %2 %) () ls))

(defn reverseR [ls]
  (accumulate #(concat %2 (list %)) () ls))


; 2.40
;; unique pairs
(defn unique-pairs [n]
   (mapcat
     (fn [x] (map #(list % x) (range 1 x)))
     (range 1 n))
  )

(defn prime-sum-pairs [n]
  (map #(cons (+ (first %) (second %)) %)
    (filter #(prime? (+ (first %) (second %)))
      (unique-pairs n))
  ))

; 2.41
;; ordered triples of i,j, k < n that sum to s
(defn unique-triples [n]
  (mapcat
    (fn [k]
      (mapcat (fn [j]
        (map #(list % j k) (range 1 j)))
        (range 1 k)
        ))
    (range 1 n)
    )
  )

(defn triple-sums [n s]
  (filter #(= (fold-left + 0 %) s) (unique-triples n)))

; 2.42
;; the eight queens

(def empty-board [])

(defn adjoin-position [r k rest-of-queens]
  (cons r rest-of-queens))

(defn safe? [k board] 
  (let [isSafe? (fn iter [n ls]
                 (cond
                   (empty? ls) true 
                   (= (first ls) (first board)) false 
                   (= (first ls) (+ (first board) n)) false 
                   (= (first ls) (- (first board) n)) false 
                   :else (iter (inc n) (rest ls)))
                 )
        ]
      (isSafe? 1 (rest board))
    ))


(defn queens [board-size]
  (let [queen-cols (fn iter [k]
                     (if (zero? k)
                       (list empty-board)
                       (filter
                         (fn [positions] (safe? k positions))
                         (mapcat
                           (fn [rest-of-queens]
                             (map (fn [new-row]
                                    (adjoin-position new-row k rest-of-queens))
                                  (range 1 (inc board-size))))
                           (iter (dec k))))))
        ]
    (queen-cols board-size)
    ))

; 2.43
;; Why does this version of queens run slower?


(defn queens [board-size]
  (let [queen-cols (fn iter [k]
                     (if (zero? k)
                       (list empty-board)
                       (filter
                         (fn [positions] (safe? k positions))
                        ;; ============================
                         (mapcat
                           (fn [new-row]
                             (map (fn [rest-of-queens]
                                    (adjoin-position new-row k rest-of-queens))
                           		(iter (dec k))))
                              (range 1 (inc board-size))))))
        ]                ;; ============================
    (queen-cols board-size)
    ))

; flatmapping across the board first changes this from tail to tree recursive, resulting in T^k time.

; 2.44
;; define an up-split procedure, as used by corner-split
(defn below [a b] :new-painter)
(defn beside [a b] :new-painter)


(defn right-split [painter n]
  (if (zero? n)
    painter
    (let [smaller (right-split painter (dec n))]
      (beside painter (below smaller smaller))
      )))



(defn up-split [painter n]
  (if (zero? n)
    painter
    (let [smaller (up-split painter (dec n))]
      (below painter (beside smaller smaller)
      ))))

;; example hof with painter ops
(defn square-of-four [tl tr bl br]
  #(let [top (beside (tl %) (tr %))
         bottom (beside (bl %) (br %))]
     (below bottom top)
     ))

; 2.45
;; define a general splitting operation
(defn split [initial child-direction]
  (fn f [painter n] 
    (if (zero? n)
      painter
      (let [smaller (f painter (dec n))]
        (initial painter (child-direction smaller smaller))
      ))
    ))

(defn corner-split [painter n]
  (if (zero? n)
    painter
    (let [up (up-split painter (dec n))
          right (right-split painter (dec n))
          (let [ top-left (beside up up)
                bottom-right (below right right)
                corner (corner-split painter (dec n))
                ]
            (beside (below painter top-left)
                    (below bottom-right corner)))
         ])))

;; example of frame coordinate map
(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect 
                  (xcor-vect v)
                  (edge-1-frame frame))
                (scale-vect
                  (ycor-vect v)
                  (edge2-frame frame))
                ))))

; 2.46
;; Implement a vector from the origin point

(defn make-vect [x y] (list x y))
(defn xcor-vect [v] (first v))
(defn ycor-vect [v] (second v))

(defn vect-combine [op]
  (fn [va vb] 
    (make-vect (op (xcor-vect va) (xcor-vect vb)) (op (ycor-vect va) (ycor-vect vb)))))

(defn add-vect [va vb] ((vect-combine +) va vb))
(defn sub-vect [va vb] ((vect-combine -) va vb))
(defn scale-vect [coef v] ((vect-combine *) v (make-vect coef coef)))

;2.47
;; Provide a frame implementation based on the constructors below

;a
(defn make-frame [origin edge1 edge2] (list origin edge1 edge2))
(defn frame-origin [frame] (first frame))
(defn frame-edge1 [frame] (second frame))
(defn frame-edge2 [frame] (nth frame 2))

;b
;; For all intents and purposes the cons implementation is identical. Only exception is that it uses car & cdr instead of first etc...

;;Painters

  (defn segments->painter [segments]
    (fn [frame]
      (for-each'
        #((draw-line
            ((frame-coord-map frame) (start-segment %))
            ((frame-coord-map frame) (end-segment %))))
        segments)))

; 2.48
;; Create constructor and accessors for line segments
(defn make-segment [start-vector end-vector] (list start-vector end-vector))
(defn segment-start [segment] (first segment))
(defn segment-end [segment] (last segment))

; 2.49
;; Define four primitive painters in terms of segments->painter
  ;a
  (defn outline-painter [frame]
    (let [w (make-segment (frame-origin frame) (frame-edge1 frame))
          s (make-segment (frame-origin frame) (frame-edge2 frame))
          n (make-segment (frame-edge1 frame) (add-vect (frame-edge2 frame) (frame-edge1 frame)))
          e (make-segment (frame-edge2 frame) (add-vect (frame-edge2 frame) (frame-edge1 frame)))
          ]
      (segments->painter `(w s n e))
      ))

;b
  (defn x-painter [frame]
    (let [o->tr (make-segment (frame-origin frame) (make-segment (frame-edge2 frame) (add-vect (frame-edge2 frame) (frame-edge1 frame))))
          tl->br (make-segment (frame-edge1 frame) (frame-edge2 frame))
          ]
      (segments->painter `(o->tr tl->br))
      ))

;c
  (defn diamond-painter [frame]
    (let [w (scale-vect 0.5 (frame-edge1 frame))
          s (scale-vect 0.5 (frame-edge2 frame))
          n (scale-vect 0.5 (add-vect (frame-edge2 frame) (frame-edge1 frame)))
          e (scale-vect 0.5 (add-vect (frame-edge2 frame) (frame-edge1 frame)))
          wn (make-segment w n)
          ne (make-segment n e)
          es (make-segment e s)
          sw (make-segment s w)
          ]
      (segments->painter `(wn ne es sw))
      ))

;d wave-painter
; todo need to think about how to structure this


; example transform painter
(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)]
      (let [new-origin (m origin)]
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin))
          )))))

(defn flip-vert [painter]
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defn shrink-to-up-right [painter]
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(defn rotate90 [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defn squish-inwards [painter]
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.65)
                     (make-vect 0.35 0.35)))

(defn beside [painter1 painter2]
  (let [split-point (make-vect 0.5 0.0)]
    (let [paint-left (transform-painter painter1
                                        (make-vect 0.0 0.0) ;; origin
                                        split-point         ;; middle of the x axis
                                        (make-vect 0.0 1.0)) ;; top left corner
          paint-right (transform-painter painter2
                                         split-point
                                         (make-vect 1.0 0.0)
                                         (make-vect 0.5 1.0))
          ]
      (fn [frame]
        (paint-left frame)
        (paint-right frame))
  )))

; 2.50
;; implement some simple painter transformations
(defn flip-horiz [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(defn rotate180 [painter]
   ((comp rotate90 rotate90) painter))

(defn rotate270 [painter]
  ((comp rotate90 rotate180) painter))

; 2.51
;; implement below for painters
  (defn below [painter1 painter2]
    (let [split-point (make-vect 0.0 0.5)]
      (let [paint-bottom (transform-painter painter1
                                             (make-vect 0.0 0.0)
                                             (make-vect 1.0 0.0)
                                             split-point)
            paint-top    (transform-painter painter2
                                             split-point
                                             (make-vect 1.0 0.5)
                                             (make-vect 0.0 0.5))
            ]
        (fn [frame]
          (paint-bottom frame)
          (paint-top frame))
        )))

; 2.52
;; modify the various levels (stratified design) of the painter system

;a wave painter


;b Change corner-split pattern
(defn corner-split [painter n]
  (if (zero? n)
    painter
    (let [up (up-split painter (dec n))
          right (right-split painter (dec n))
          (let [corner (corner-split painter (dec n))]
            (beside (below painter up)
                    (below right corner)))
         ])))

;c change the square-limit structure


; 2.53
;;what gets printed

(list 'a 'b 'c)
; (a b c)

(list (list 'george))
; (george)

(cdr '((x1 x2) (y1 y2)))
; (y1 y2)

(cadr '((x1 x2) (y1 y2)))
; x2

(pair? (first '(a short list)))
; false

(memq 'red '((red shoes) (blue socks)))
; nil

(memq 'red '(red shoes blue socks))
; (red shoes blue socks)

; 2.54
;;list equality
(defn list-equal [xs ys]
  (if (empty? xs)
    (empty? ys)
    (let [xh (first xs)
          yh (first ys)]
      (and (if (and (list? xh) (list? yh))
             (list-equal xh yh)
             (= xh yh))
           (list-equal (rest xs) (rest ys)))
    )))

; 2.55
;; Why does (first ''abracadabra) print quote?

;; ' is shorthand for the quote procedure, so the quote of quote is '(quote ....). THis is why ''abcr... turns into '(quote abcdrea), and why first pulls out the quote literal


; Symbolic diferentiation
(defn triple? [col] (= (count col) 3))

(defn variable? [e] (symbol? e))
(defn same-variable? [a b] (and (variable? a) (variable? b) (= a b)))
(defn =number? [exp num] (and (number? exp) (= exp num)))

(defn make-sum [l r]
  (cond
    (=number? l 0) r
    (=number? r 0) l
    (and (number? l) (number? r)) (+ l r)
    :else (list '+ l r)
    ))
(defn addend [sum] (second sum))
(defn augend [sum] (nth sum 2))
(defn sum? [x] (= (first x) '+))

(defn make-product [l r]
  (cond
    (or (=number? l 0) (=number? r 0)) 0
    (=number? l 1) r
    (=number? r 1) l
    (and (number? l) (number? r)) (* l r)
    :else (list '* l r)
  ))
(defn multiplier [product] (second product))
(defn multiplicand [product] (nth product 2))
(defn product? [x] (= (first x) '*))




; 2.56
;; Extend deriv to support this rule: d(u^n)/ dx = nu^(n-1) * (du/dx)
(defn make-exponentiation [num term]
  (cond
    (=number? term 0) 1
    (=number? term 1) num
    (or (= num 1) (zero? num)) num
    :else (list '** num term)
    ))
(defn base [exp] (second exp))
(defn exponent [exp] (nth exp 2))
(defn exponent? [x] (and (triple? x) (= (first x) '** )))

(defn deriv [exp var]
  (cond
    (number? exp) 0
    (variable? exp)
      (if (same-variable? exp var)
        1
        0)
    (sum? exp)
      (make-sum
        (deriv (addend exp) var)
        (deriv (augend exp) var))
    (product? exp)
      (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp))
        )
    (exponent? exp)
      (make-exponentiation (make-product
                             (exponent exp)
                             (make-exponentiation
                               (base exp)
                               (dec (exponent exp))))
                           (deriv (base exp) var))
    :else  (do
             (println exp)
             nil
             )
    ))

; 2.57
;; update the program to handle arbitrary sized sum and product expressions

(defn augend [s]
  (let [a (rest (drop 1 s))]
    (if (= (count a) 1)
      a
      (cons '+ a)
      )))

(defn multiplicand [p]
  (let [a (rest (drop 1 p))]
    (if (= (count a) 1)
      a
      (cons '* a))
    ))

; 2.58
;; update the diferentiation program to work on infix data rather than prefix. Also build in the order of operations

;a

(defn make-sum [l r]
  (cond
    (=number? l 0) r
    (=number? r 0) l
    (and (number? l) (number? r)) (+ l r)
    :else '(l + r)
    ))
(defn addend [sum] (first sum))
(defn augend [sum] (nth sum 2))
(defn sum? [x] (= (second x) '+))

(defn make-product [l r]
  (cond
    (or (=number? l 0) (=number? r 0)) 0
    (=number? l 1) r
    (=number? r 1) l
    (and (number? l) (number? r)) (* l r)
    :else (list l '* r)
    ))
(defn multiplier [product] (first product))
(defn multiplicand [product] (nth product 2))
(defn product? [x] (= (second x) '*))

;b
; this is where it gets complicated thanks to order of operations

; check for multiplication first, walking left -> right
(defn product? [ls] (not (nil? (some #{'*} ls))))
(defn sum? [ls] (not (nil? (some #{'+} ls))))

(defn right-of-sym [sym expr]
  (let [r (rest (drop-while #(not (= sym %)) expr))]
    (if (= 1 (count r))
      (first r)
      r
      )))

(defn left-of-sym [sym expr]
  (let [l (take-while #(not (= sym %)) expr)]
    (if (= 1 (count l))
      (first l)
      l
      )))

(defn addend [sum] (left-of-sym '+ sum))
(defn auggend [sum] (right-of-sym '+ sum))
(defn multiplier [prod] (left-of-sym '* prod))
(defn multiplicand [prod] (right-of-sym '* prod))

(defn make-product [l r]
  (cond
    (or (=number? l 0) (=number? r 0)) 0
    (= 1 l) r
    (= 1 r) l
    (and (number? l) (number? r)) (* l r)
    (and (coll? l) (coll? r) (not (sum? l)) (not (sum? r)))  (list l '* r)
    (and (coll? l) (not (sum? l))) (concat l ['* r])
    (and (coll? r) (not (sum? r))) (concat [l '*] r)
    :else (list l '* r)
    ))

(defn make-sum [l r]
  (cond
    (=number? l 0) r
    (=number? r 0) l
    (and (number? l) (number? r)) (+ l r)
    (and (coll? l) (coll? r)) (list l '+ r)
    (coll? l) (concat l ['+ r])
    (coll? r) (concat [l '+] r)
    :else (list l '+ r)
    ))


;2.59
;; Set operations
(defn element-of-set? [elem set]
  (cond
    (empty? set) false
    (= x (first set)) true
    :else (element-of-set? elem (rest set))
    ))

(defn adjoin-set [elem set]
  (if element-of-set? elem set) set (cons elem set))

(defn intersection-set [set1 set2]
  (cond
    (or (empty? set1) (empty? set2)) '()
    (element-of-set? (first set1) set2) (cons (first set1) (intersection-set (rest set1) set2))
    :else (intersection-set (rest set1) set2)
    ))

(defn union-set [set1 set2]
  (cond
    (empty? set1) set2
    (empty? set2) set1
    (element-of-set? (first set1) set2) set2
    :else (union-set (rest set1) (cons (first set1) set2))
    ))


;2.60
;; allow duplicate elements in the sets
(defn adjoin-set [elem set] (cons elem set))

(defn union-set [set1 set2]
  (cond
    (empty? set1) set2
    (empty? set2) set1
    :else (union-set (rest set1) (cons (first set1) set2))
    ))

;; as should be clear from the functions I changed, removing the uniqueness constraint on the set's internal representation
;; allows for very fast append & union, at the expense of slower lookups via element & intersection. This would be
;; useful in append-heavy workflows where searches are comparatively light

;2.61
;; Adjoin-set for ordered list implementation of sets
(defn element-of-set-o [x set]
  (cond
    (empty? set) false
    (= x (first set)) true
    (< x (first set)) false
    :else (element-of-set-o x (rest set))
  ))

(defn intersection-set-o [set1 set2]
  (if (or (empty? set1) (empty? set2))
    '()
    (let [x1 (first set1)
          x2 (first set2)]
      (cond
        (= x1 x2) (cons x1 (intersection-set-o (rest set1) (rest set2)))
        (< x1 x2) (intersection-set-o (rest set1) set2)
        (< x2 x1) (intersection-set-o set1 (rest set2))
        ))
    ))

(defn adjoin-set-o [x set]
  (if (empty? set)
    (cons x set)
    (concat
      (take-while #(< % x) set)
      (let [tail (drop-while #(< % x) set)]
        (if (= x (first tail))
          tail
          (cons x tail)
        )
      )
    )))

;2.62
;; provide an O(n) union-set for ordered list set implementation
(defn union-set-o [set1 set2]
  (cond
    (empty? set1) set2
    (empty? set2) set1
    :else (union-set-o (rest set1) (adjoin-set-o (first set1) set2))
    )
  )

;; Binary Trees

(defn entry [tree] (first tree))
(defn left-branch [tree] (second tree))
(defn right-branch [tree] (nth tree 2))
(defn make-tree [entry left right] [entry left right])

(defn element-of-set? [x set]
  (cond
    (empty? set) false
    (= x (entry set)) true
    (< x (entry set)) (element-of-set? x (left-branch set))
    (> x (entry set)) (element-of-set? x (right-branch set))
    ))

(defn adjoin-set [x set]
  (cond
    (empty? set) (make-tree x '[] '[])
    (= x (entry set)) set
    (< x (entry set))
      (make-tree (entry set)
                 (adjoin-set x (left-branch set))
                 (right-branch set))
    (> x (entry set))
      (make-tree (entry set)
                 (left-branch set)
                 (adjoin-set x (right-branch set)))
    ))

(def simpleTree (make-tree 7
                           (make-tree 3
                                      (make-tree 1 '[] '[])
                                      (make-tree 5 '[] '[]))
                           (make-tree 9
                                      '[]
                                      (make-tree 11 '[] '[]))
                           ))

(def unbalancedTree [3
                     [1 '[] '[]]
                     [7
                      [5 '[] '[]]
                      [9 '[]
                       [11 '[] '[]]
                       ]
                      ]
                     ])

(def flippedSimpleTree [5
                        [3
                         [1 '[] '[]]
                         '[]]
                        [9
                         [7 '[] '[]]
                         [11 '[] '[]]
                         ]
                        ]
  )



; 2.63
;; Are these two binary-tree => list functions equivalent?

(defn tree->list [tree]
  (if (empty? tree)
    '[]
    (concat
      (tree->list (left-branch tree))
      (cons (entry tree) (tree->list (right-branch tree)))
      )))

(defn tree->list' [tree]
  (defn copy-to-list [tree result-list]
    (if (empty? tree)
      result-list
      (copy-to-list
        (left-branch tree)
        (cons (entry tree)
              (copy-to-list (right-branch tree) result-list))
        )))
  (copy-to-list tree '[])
  )

;a
;; Will tree->list & tree-list' produce the same tree for every tree?
;; Yes, these will produce equivalent trees. Both functions recursively walk the trees and build up from l -> right

;b
;; Do tree->list & tree->list' have the same order of growth?
;; No, the first procedure, because it uses concat, requires O(n log n) steps due to concat repeatedly walking smaller & smaller left-hand lists for each input (hence the log(n))
;; The second procecure completes in linear time because it grows the list by consing a single element onto the result-list for each sub-tree, avoiding repeated traversals

; 2.64
;; convert a list to a tree

(defn list->tree [ls]
  (first (partial-tree ls (count ls))))

(defn partial-tree [ls n]
  (if (zero? n)
    (cons '[] ls)
    (let [left-size (quot (dec n) 2)]
      (let [left-result (partial-tree ls left-size)]
        (let [left-tree (first left-result)
              non-lefts (rest left-result)
              right-size (- n (inc left-size))]
          (let [this-entry (first non-lefts)
                right-result (partial-tree (first non-lefts) right-size)]
            (let [right-tree (first right-result)
                  remaining (rest right-result)]
              (cons (make-tree this-entry left-tree right-tree) remaining)
            )))))))

;a
;; explain how this function works
;; partial-tree works by starting from the midpoint of the list and building up the left & right sub trees recursively.
;; It does this by repeatedly taking smaller & smaller slices from the original list on both the left & right side until there is nothing left, at which point it returns an empty vector,
;; which is then uses as the fixed-point of the function & added to the appropriate place in the tree.

;;                   5
;;                 /   \
;;                1     9
;;                 \   / \
;;                  3 7   11

;b
;; The procedure grows with linear time since it only requires a single traversal across the input list. All other operations are constant time or on an as-yet untouched subset of the data.

; 2.65
;; provide O(n) implementations of union & intersection on balanced binary trees

(defn union-set [set1 set2]
  (list->tree (union-set-o (tree->list' set1) (tree->list' set2))))

(defn intersect-set [set1 set2]
  (list->tree (intersection-set-o (tree->list' set1) (tree->list' set2))))


; 2.66
;; Iplement key lookup for random access
(defn lookup [key set]
  (cond
    (empty? set) false
    (= (entry set) key) (entry set)
    (< key (entry set)) (lookup key (left-branch set))
    (> key (entry set)) (lookup key (right-branch set))
    :else false
    ))


;; to encode n characters, you need roughtly log2n bits per symbol

; Huffman trees
(defn make-leaf [symbol weight] (list 'leaf symbol weight)) ; Interesting way to make this "typed"
(defn leaf? [node] (= (first node) 'leaf))
(defn symbol-leaf [node] (second node))
(defn weight-leaf [node] (nth node 2))

(defn syms [node]
  (if (leaf? node)
    (list (symbol-leaf node))
    (nth node 2)))

(defn weight [node]
(if (leaf? node)
  (weight-leaf node)
  (last node)
  ))

(defn make-code-tree [left right]
  (list left
        right
        (concat (syms left) (syms right))
        (+ (weight left) (weight right))
        ))
(defn left-branch [tree] (first tree))
(defn right-branch [tree] (second tree))

(defn choose-branch [bit node]
  (if (zero? bit)
    (left-branch node)
    (right-branch node))
  )

(defn decode [bits tree]
  (defn decode-symbol [bits current-branch]
    (if (empty? bits)
      '()
      (let [next-branch (choose-branch (first bits) current-branch)]
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch) (decode-symbol (rest bits) tree))
          (decode-symbol (rest bits) next-branch)
          ))
      ))
  (decode-symbol bits tree)
  )

(defn adjoin-set [x set]
  (cond
    (empty? set) (list x)
    (< (weight x) (weight (first set))) (cons x set)
    :else (cons (first set) (adjoin-set x (rest set)))
        ))

(defn make-leaf-set [pairs]
  (if (empty? pairs)
    '()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair) (second pair)) (make-leaf-set (rest pairs)))
      )))

; 2.67
;; create an encoding tree and simple message using the Huffman tree code
(def sample-tree                                            ;   ({A, B, C, D}, 8)
  (make-code-tree                                           ;         / \
    (make-leaf 'A 4)                                        ;        /   \
    (make-code-tree                                         ;   (A, 4)  ({B, C, D}, 4)
      (make-leaf 'B 2)                                      ;           /    \
      (make-code-tree                                       ;     (B, 2)    ({C, D}, 2)
        (make-leaf 'D 1)                                    ;              /     \
        (make-leaf 'C 1)))))                                ;         (C, 1)     (D, 1)

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;Result is (A D A B B C A)

; 2.68
;; encode a message

(defn encode-symbol [symbol tree]
  (defn go [bits sub-tree]
      (cond
        (leaf? sub-tree) bits
        (and (leaf? (left-branch sub-tree)) (= symbol (symbol-leaf (left-branch sub-tree) )))(go (conj bits 0) (left-branch sub-tree))
        (and (leaf? (right-branch sub-tree)) (= symbol (symbol-leaf (left-branch sub-tree))))(go (conj bits 1) (right-branch sub-tree))
        (not (nil? (some #{symbol}  (syms (left-branch sub-tree)))))(go (conj bits 0) (left-branch sub-tree))
        (not (nil? (some #{symbol} (syms (right-branch sub-tree)))))(go (conj bits 1) (right-branch sub-tree))
        ))
    (go '[] tree)
 )


(defn encode [message tree]
  (if (empty? message)
    '()
    (concat (encode-symbol (first message) tree)
            (encode (rest message) tree))))

; 2.69
;; symbol frequency pairs to generate a huffman tree

(defn successive-merge [trees]
  (if (= 1 (count trees))
    (first trees)
    (let [l (first trees)
          r (second trees)
          more (drop 2 trees)
          tree (make-code-tree l r)
          trees (adjoin-set tree more)
          ]
      (successive-merge trees)
      )))
;;grab first two pairs, turn into a tree, then use adjoin set to preserve ordering correctly

(defn generate-huffman-tree [pairs] (successive-merge (make-leaf-set pairs)))


; 2.70
;; encode and decode the rock song

(def rock-pairs
  '((na 16) (yip 9) (sha 3) (a 2) (get 2) (job 2) (boom 1) (wah 1)))

(def rock-tree (generate-huffman-tree rock-pairs))



(def rock-message '(get a job
                      sha na na na na na na na na
                      get a job
                      sha na na na na na na na na
                      wah yip yip yip yip yip yip yip yip yip
                      sha boom))

(def encoded-tree (encode rock-message rock-tree))


;; a
(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
;;b
;; if this were a fixed-length encoding then it would require k * n bits. for an alphabet of 8 symbols, that's 3 bits per symbol, and the message length is 36, so 36*3, or 108 bits.

; 2.71
;; Huffman tree sizes
;; A Huffman tree always uses a single bit for the most common symbol in the alphabet & it will use n-1 bits for the least common symbol, so it should end up at n log n bits

; 2.72
;; What is the order of growth for the code in 2.68
;; Should be something akin to O(n^2) because of repeated searches and the cost of walking the symbol list.


;; Data driven programming
;; aka a sad type-system in Clojure

;2.73
;; Rework the symbolic diferentiation program from earlier to use a type lookup table

(defn operator [exp] (first exp))
(defn operands [exp] (rest exp))

(defn deriv [exp var]
  (cond
    (number? exp) 0
    (variable? exp)
    (if (same-variable? exp var) 1 0)
    :else  ((get-lookup-table 'deriv (operator exp)) (operands exp) var)
    ))

;a
;; The above coe factors out all of the logic releavant to particular symbols & hides it away behind the get function,
;; which will perform a lookup into said symbol table to pull out the correct function.

;b
(defn make-sum [l r]
  (cond
    (=number? l 0) r
    (=number? r 0) l
    (and (number? l) (number? r)) (+ l r)
    :else (list '+ l r)
    ))

(defn make-product [l r]
  (cond
    (or (=number? l 0) (=number? r 0)) 0
    (=number? l 1) r
    (=number? r 1) l
    (and (number? l) (number? r)) (* l r)
    :else (list '* l r)
    ))

(defn addend [exp] (first exp))
(defn augend [exp] (second exp))
(defn multiplier [exp] (first exp))
(defn multiplicand [exp] (second exp))

(defn deriv-sum [exp var]
  (make-sum
    (deriv (addend exp) var)
    (deriv (augend exp) var)))

(defn deriv-mult [exp var]
  (make-sum
    (make-product (multiplier exp)
                  (deriv (multiplicand exp) var))
    (make-product (deriv (multiplier exp) var)
                  (multiplicand exp))))

(def lookup-table (atom {}))
(defn get-lookup-table [op t]
  (@lookup-table [op t]))
(defn put-lookup-table [op t f]
  (swap! lookup-table #(assoc % [op t] f)))


; c
(defn make-exponentiation [num term]
  (cond
    (=number? term 0) 1
    (=number? term 1) num
    (or (= num 1) (zero? num)) num
    :else (list '** num term)
    ))
(defn base [exp] (first exp))
(defn exponent [exp] (second exp))

(defn load-package []
  ((put-lookup-table 'deriv '+ deriv-sum)
    (put-lookup-table 'deriv '* deriv-mult)
    (put-lookup-table 'deriv '** deriv-exponent)
    ))

(defn deriv-exponent [exp var]
  (make-exponentiation (make-product
                         (exponent exp)
                         (make-exponentiation
                           (base exp)
                           (dec (exponent exp))))
                       (deriv (base exp) var))
  )

;d
;; the only change that should be made if we flip the deriv & symbol would be changing the lookup table construction
;; to use symbol then deriv. This is trivial thanks to the underlying map.

;2.74
;; Simple file system using data directed programming

;a
(defn division [f] (first f))
(def file-lookup-table (atom {}))

(defn get-employee-record [file]
  (let [div (division file)]
    (file-lookup-table [div 'empId] file)
    ))

;; each division's files must have an accessor function for extracting employee ids

;b
(defn get-employee-salary [file]
  (let [div (division file)]
    (file-lookup-table [div 'salary] file)
    ))

;; again, the division's file now needs a salary accessor as well. Otherwise the symbol lookup table handles this

;c
(defn employee-name [div record]
  (file-lookup-table [div 'empName] record))

(defn find-employee-record [divFiles empName]
  (let [ empRecord (filter (fn [f] (= empName (employee-name (division f) (get-employee-record f)))) divFiles)]
  (if (empty? empRecord) () (first empRecord))
))

;d
;; When a new company is gobbled up by insatiable, they only need to have the new company install their own file package
;; into the main system.

; 2.75
;; implement make-from-mag-ang in message passing style

(defn make-from-mag-ang [mag ang]
  (defn dispatch [op]
    (cond
      (= op 'magnitude) mag
      (= op 'angle) ang
      (= op 'real-part) (* mag (Math/cos ang))
      (= op 'imag-part) (* mag (Math/sin ang))
      :else ()
    )))

; 2.76
;; Message passing seems to handle adding new types quite well, while data-directed supports adding new operations better

; 2.77
;; Describe why the following code works for selecting parts of the complex numbers
(put-lookup-table 'real-part '(complex) real-part)
(put-lookup-table 'imag-part '(complex) imag-part)
(put-lookup-table 'magnitude '(complex) magnitude)
(put-lookup-table 'angle '(complex) angle)

;a
; Adding these functions to the lookup table will make them available for complex types because it exposes them as part of the
; public api for the system. Otherwise those calls are hidden within the complex scope.

;b
;Trace through the procedure execution
; There should be two calls to apply-generic, one for complex & one for rectangular wrt. magnitude

; 2.78
;; update the type tag system to treat scheme numbers normally rather than as a pair
(def numeric-lookup (atom {}))
(defn get-numeric [op t]
  (@numeric-lookup [op t]))
(defn put-numeric [op t f]
  (swap! numeric-lookup #(assoc % [op t] f)))

(defn install-scheme-numbers []
  (put-numeric 'add '(scheme-number) +)
  (put-numeric 'sub '(scheme-number) -)
  (put-numeric 'mul '(scheme-number) *)
  (put-numeric 'div '(scheme-number) /)
  'done
  )

;; This will check whether the arguments are all numbers, and if so look up the appropriate operator
(defn apply-generic [op & args]
  (cond
    (every? #(number? %) args) (apply (get-numeric op '(scheme-number)) args)
    ;...
    )
  )

; 2.79
;; generic equality predicate
(defn install-scheme-numbers []
  (put-numeric 'add '(scheme-number) +)
  (put-numeric 'sub '(scheme-number) -)
  (put-numeric 'mul '(scheme-number) *)
  (put-numeric 'div '(scheme-number) /)
  (put-numeric 'eq '(scheme-number) =)
  'done
  )

(defn install-rational-numbers []
  (defn numer [x] (first x))
  (defn denom [x] (second x))
  (defn make-rat [n d]
    (let [g (gcd n d)]
      (cons (/ n g) (/ d g))))

  (defn add-rat [x y]
    (make-rat (+ (* (numer x) (denom y) (* (numer y) (denom x))))
              (* (denom x) (denom y))))
  (defn sub-rat [x y]
    (make-rat (- (* (numer x) (denom y) (* (numer y) (denom x))))
              (* (denom x) (denom y))))
  (defn mul-rat [x y]
    (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
  (defn div-rat [x y]
    (make-rat (* (numer x) (denom y )) (* (numer y) (denom x))))
  (defn eq-rat [x y]
    (let [xr (make-rat (numer x) (denom x))
          yr (make-rat (numery y) (denom y))]
      (and (= (numer xr) (numer yr)) (= (denom xr) (denom yr)))))

  (defn tag [x] (attatch-tag 'rational x))
  (put-numeric 'add '(rational rational) #(tag (add-rat %1 %2)))
  (put-numeric 'sub '(rational rational) #(tag (sub-rat %1 %2)))
  (put-numeric 'mul '(rational rational) #(tag (mul-rat %1 %2)))
  (put-numeric 'div '(rational rational) #(tag (div-rat %1 %2)))
  (put-numeric 'eq '(rational rational) #(tag (eq-rat %1 %2)))
  )

;; feeling lazy, but assume this pattern is repeated for complex numbers, with this for equality

(defn eq-complex [x y] (and (= (real-part x) (real-part y) (= (imag-part x) (imag-part y)))))
(put-numeric 'eq '(complex complex) eq-complex)

; 2.80
;; Add a zero check for all three.
;; I'll be doing the abbreviated implementation again, no more writing out the world.

(put-numeric '=zero? '(scheme-number) zero?)
(defn =zero?-rat [x] (zero (numer x)))
(put-numeric '=zero? '(rational rational) =zero?-rat)
(defn =zero?-complex [x] (and (zero? (real-part x)) (zero? (imag-part x))))
(put-numeric '=zero? '(complex complex) =zero?-complex)

; 2.81
;; self-coercion issues

;a If we install self-coercive functions into the lookup table, what happens if two arguments are passed in for
;; a function that they don't support?

;; we will do extra work by self-coercing the calls to themselves before falling down at the end and recognizing there's no function for the argument types

;b He was incorrect. The first step looks up for a function & some type tags, and if the function is found then it applies it, otherwise we go through the coercion
;c

(defn apply-generic [op & args]
  (let [type-tags ((map #(first %) args))]
    (let [proc (get-numeric op type-tags)]
      (if proc
        (apply proc (map #(contents %) args))
        (if (= (count args) 2)
          (let [type1 (first type-tags)
                type2 (second type-tags)
                a1 (first args)
                a2 (second args)]
            (if (= type1 type2)
              (throw (Exception "not supported for these types"))
              (let [t1->t2 (get-coercion type1 type2)
                    t2->t1 (get-coercion type2 type1)]
                (cond
                  (t1->t2) (apply-generic op (t1->t2 a1) a2)
                  (t2->t1) (apply-generic op (t2->t1 a2) a1)
                  :else (throw (Exception "not supported for these types"))
                  )
                ))))))))

; 2.82
;; generalize apply-generic to handle multiple arguments in the general case
;; The trick is to find the least-general common type & use that

(def type-hierarchy (atom {}))
(def root-type 'root)
(defn common-supertype [t1 t2]
  (let [tag-list1 (@type-hierarchy t1)
        tag-list2 (@type-hierarchy t2)]
    (defn firstCommon [remaining checkList]
      (let [h (first remaining)
            t (rest remaining)]
        (cond
          (contains? checkList h) h
          (not (empty? t)) (firstCommon t checkList)
          :else root-type
          )))
    (firstCommon tag-list1 tag-list2)
    )
  )

(defn common-list-type [ls]
  (cond
    (empty? ls) root-type
    (empty? (next ls)) (first ls)
    (= 2 (length ls)) (common-supertype (first ls) (second ls))
    :else
      (fold-left common-supertype (common-supertype (first ls) (second ls)) (next (next ls)))
    ))

(defn apply-generic-multi [op & args]
  (let [type-tags ((map #(first %) args))]
    (let [shared-type (common-list-type type-tags)]
      (let [coerceFuncs (map #(get-coercion % shared-type) type-tags)
            mapped (map vector coerceFuncs args)
            coerced (map #((first %) (second %)) mapped)
            ]
        (apply-generic op shared-type coerced)
        )
      )
    )
  )

;2.83

(defn install-hierarchy []
  (swap! type-hierarchy #(assoc % 'complex '(root)))
  (swap! type-hierarchy #(assoc % 'real '(complex root)))
  (swap! type-hierarchy #(assoc % 'rational '(real complex root)))
  (swap! type-hierarchy #(assoc % 'integer '(rational real complex root)))
  )

(defn raise [type]
  (let [direct-parent (first (@type-hierarchy type))
        f (get-coercion type direct-parent)]
    (f type))
  )

;;This pulls the coercion from our type to its direct parent. For complex, the direct parent is root, to which there cannot be a coercion.
;; Hence, we can now raise any type in our hierarchy

;2.84
(defn raiseTo [obj target-type]
  (if (= (first obj) target-type)
    obj
    (raise obj)
    ))

(defn apply-generic-raising [op & args]
  (let [type-tags ((map #(first %) args))]
    (let [shared-type (common-list-type type-tags)]
      (let [mapped (map #(raiseTo % shared-type) args)]
        (apply-generic op shared-type mapped)
      ))))

;2.85
;; implement project, which pushes a type down the hierarchy
(defn project [type]
  (let [direct-child (filter #(= type (first (second %))) (seq type-hierarchy))
        f (get-projection type direct-child)]
    (f type)))

(defn can-drop-type? [obj]
  (let [t (first obj)]
    (cond
      (= t 'rational) (not (zero? (denom obj)))
      (= t 'real) (rational? obj)
      (= t 'complex) (=zero?-complex (imag-part obj))
      )
    )
  )

(defn drop [obj]
  (if can-drop-type? obj)
    (let [tpe (first obj)]
       ((project tpe) obj)
      )
    obj
  )

;2.86
;; support complex numbers where each part may be any sort of number
;; This basically includes implementing euler's formula

(defn sine-gen [num] (apply-generic 'sine num))
(defn cosine-gen [num] (apply-generic 'cosine num))

(defn install-sine-cosine []
  (put-numeric 'sine '(rational ) #(tag (Math/sin (/ (numer %) (denom %)))))
  (put-numeric 'cosine '(rational ) #(tag (Math/cos (/ (numer %) (denom %)))))
  (put-numeric 'sine '(number) #(tag (Math/sin %1)))
  (put-numeric 'cosine '(number) #(tag (Math/cos %1)))
  )

;; this is pretty involved, obviously. todo come back and implement this

(defn add-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (add-terms (term-list p1)
                          (term-list p2)))
    (throw (RuntimeException "not a polynomial"))
    ))

(defn mul-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (mul-terms (term-list p1)
                          (term-list p2)))
    (throw (RuntimeException "not a valid polynomial"))
    ))

(defn make-poly [variable term-list] (cons variable term-list))
(defn variable [p] (first p))
(defn term-list [p] (rest p))
(defn empty-termlist? [p] (empty? p))
(def the-empty-termlist '())
(defn adjoin-term [terms p] (cons p terms))
(defn order [term] (first term))
(defn coeff [term] (rest term))
(defn first-term [terms] (first terms))
(defn rest-term [terms] (rest terms))
(defn adjoin-term [term terms]
  (if (=zero? (coeff term))
    terms
    (cons term terms)
    ))

(defn install-polynomial-package
  (defn tag [p] (attatch-tag 'polynomial p))
  (put-numeric 'add '(polynomial polynomial) #(add-poly %1 %2))
  (put-numeric 'mul '(polynomial polynomial) #(mul-poly %1 %2))
  (put-numeric 'make 'polynomial #((tag (make-poly %1 %2))))
  )


;2.87
;; install zero for polynomials into the generic arithmetic package

(defn zero-terms? [terms]
  (if (empty-termlist? terms)
    true
    (and (=zero? (coef (first-term terms)))
         (zero-terms? (rest-terms terms)))
    ))

(defn poly-zero? [p] (zero-terms? (term-list p)))
(defn =zero? [p] (poly-zero? p))


; 2.88
;; implement polynomial subtraction using negation
;; obvious fact here is a-b = a + (- b)

(defn negate [a] (apply-generic 'negate a))
(defn poly-map [f terms] (map f terms))
(defn sub-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (add-terms (term-list p1)
                          (make-poly p2 (poly-map negate p2))))
    (throw (RuntimeException "different poly variables"))
    ))

;;todo come back and finish the polynomials




