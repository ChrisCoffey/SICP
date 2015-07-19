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

;; addition and subtrction deal directly with the same positoins between two intervals a & b, so taking the sum or difference
;; between a & b is equivalent to 2(w(a) + w(b))
