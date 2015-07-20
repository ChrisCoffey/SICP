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
  (map f ls))


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
