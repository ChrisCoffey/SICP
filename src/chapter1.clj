(ns sicpExercises.chapter1)

;Exercise 1.2
; 5 + 4 + (2 - ( 3 - ( 6 + 5/4))) / 3 * (6 - 2) * (2 - 7)

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; 1.3
; Sum of two largest squares given 3 arguments
(defn sumOfSquares [a b c]
  (letfn [(square [n] (* n n ))]
    (- (apply + (map square (vector a b c)))
      (square (min a b c)))
  ))

(defn sumOFSquaresSimple [a b c]
  (letfn [(square [n] (* n n))]
    (apply +  (take 2 (map square (sort (vector a b c))))
    )))

; 1.4
; Describe behavior of following expression
(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))

;; This function takes a pair of arguments and adds the first to the absolute value of the second.
;; The fact that we use applicative order means that b is evaluated first, found to be negative, then passed into the
;; conditional operator and checked against 0

; 1.5
; Given some function p & a test function shown below, what happens when the input (test 0 (p)) is provided?
(defn test [a b]
  (if (= a 0) 0 b))

;; This will return 0 without b ever being evaluated because it is not necessary for an un-used variable to be evaludated
;; in normal order evaluation.

; 1.6
; What happens when the new version of if is evaluated?

;; Given that we use applicative ordering, the second statement will never terminate because sqrt-iter is recursive
;; and will evaluate forever, while If apparently has some baked in logic to prevent this

; 1.7
; Define a better good-enough? call for square-root checking
(defn abs [x] (* ((if (< x 0.0) - +) x)))

(defn pctDiff [x y]
  (abs (/ (- x y) (+ y 0.0))))

(defn good-enough? [guess prev]
  (if (< (pctDiff guess prev) 0.1)
    true
    false
    )
  )

(defn bad-good-enough? [guess prev]
  (if (< (abs (- guess prev )) 0.1)
  true
  false))

;; For very small numbers like .00001 or something along those lines, the bad version of good enough will be far to permissive
;; and return sqaure root values too far ffrom the actual value. Conversely, for large numbers a pure distance-based metric
;; may never converge close enough, so the function will never terminate. In the percentage based approach we can handle
;; both of these cases correctly by normalizing to an acceptable range (percentages). This guarantees termination and accuracy
;; regardless of the input


; 1.8
; Newton's method for cube roots
;; (x/ y^2 + 2y )/ 3

;(defn sqrt [guess x]
;  (if (good-enough? guess x)
;    guess
;    (letfn  average [a b] (/ (+ a b) 2)
;      (sqrt (average guess (/ x guess)) x))))


(defn cubeMinimize [guess x]
  (/ (+ (/ x (* guess guess)) (* guess 2.0))
    3))

(defn cubert [guess x]
  (let [next (cubeMinimize guess x)]
    (if (good-enough? next guess )
      next
      (cubert  next x)
    )
  ))

; Quick port of the factorial function using recursion
(defn ! [n]
  (if (= n 0)
    1
    (* n (! (- n 1)))))

; 1.9
;; Given two addition implementations:
; 1
(defn add [a b]
   (if (= a 0)
     b
     (add (dec a) (inc b))))

; 2
(defn add [a b]
  (if (= a 0)
    b
    (inc (add (dec a) b))))

;; impl 1 is itterative. It will transition one from a to be until a is 0, essentially using a as the max counter
;; Impl 2 is recurstive.

; 1.10
; Ackerman's function
(defn Ackermans [x y]
  (cond
    (= y 0) 0
    (= x 0) (* 2 y)
    (= y 1) 2
    :else (Ackermans (- x 1) (Ackermans x (- y 1)))))

; Ackermans results in 2^(n^2), which is a pretty rapid explosion

;; Example: Change counting
;; Given the coins .01, .05, .1, .25, .5, how many ways are there to make 1.00?
;; A simplified version would be making change for .1 using .01 & .05

(defn countChange [amount]
  (let [firstDenom (fn [kc]
            (cond
              (= kc 1) 1
              (= kc 2) 5
              (= kc 3) 10
              (= kc 4) 25
              (= kc 5) 50))
        cc (fn ic [amount kindsOfCoins]
             (cond
               (zero? amount) 1
               (or (< amount 0) (zero? kindsOfCoins)) 0
               :else (+ (ic amount (dec kindsOfCoins))
                        (ic (- amount (firstDenom kindsOfCoins)) kindsOfCoins))))
        ]
    (cc amount 5)))

; 1.11
; function f(n) = if(n < 3) n else f(n-1) + 2f(n-2) + 3f(n-3)

;; Recursive solution
(defn recF [n]
  (if (< n 3)
    n
    (+ (recF (dec n)) (* 2 (recF (- n 2))) (* 3 (recF (- n 3))))))

;; Iterative solution
;; process is:
;; f(1) = 1
;; f(2) = 2
;; f(3) = 4
;; f(4) = 11
;; f(5) = 25
;; f(6) = 59
;; f(7) = 142
;; f(8) = 335
;; f(9) = 796
;; f(10) = 1892 = 796 + 2(335)
(defn F [n]
  (letfn [(iter [one two three count]
    (cond
      (< n 3) n
      (= count n ) (+ one (* 2 two) (* 3 three))
      :else (iter
              (+ one (* 2 two) (* 3 three))
              one
              two
              (inc count))))]
  (iter 2 1 0 3))
  )

; 1.12
;; Recursively compute the [r,n]th element of pascal's triangle.
;; Pascals triangle
;;;          1
;;;         1 1
;;;        1 2 1
;;;       1 3 3 1
;;;      1 4 6 4 1
;;;    1 5 10 10 5 1
;;;    .............

(defn pascal-element [row column]
  (cond
    (or (zero? row) (zero? column)) 1
    (= row column) 1
    :else (+ (pascal-element (dec row) (dec column)) (pascal-element (dec row) column))))

;1.13
;; Prove Fib(n) is the closest integer to phi^n/sqrt(5), where phi = golden ratio

(defn fibProof [n]
  (let [phi (/ (+ 1 (Math/sqrt 5.0)) 2)
        psi (/ (- 1 (Math/sqrt 5.0)) 2)
        fibN (fn fib [n]
               (cond
                 (or (= n 1) (= n 2)) 1
                 :else (+ (fib (dec n)) (fib (- n 2)))))]
    [(fibN n) (/ (- (Math/pow phi n) (Math/pow psi n)) (Math/sqrt 5))]
    ))

; 1.14
;; Draw the tree for computing the ways to make change for 11 cents

;;Given that there are three types of coins less than 11 cents, we start our algorithm with KC = 3
;;                          (cc 11 3)
;;                          /
;;                (cc 11 2) + (cc 1 3)
;;                   /                \
;;        (cc 11 1) + (cc 6 3)          1
;;                /     \
;;      (11 0) + (10 1)    (1 3)
;;        /       \             \
;;      0     (10 0) + (9 1)     ...
;;            /         \           \
;;           0         ...           1
;;                        \
;;                          1

; 1.15
;; Sine of an angle in radians leverages sin x = 3 sin x/3 - 4 sin^3 x/3

(defn sine [angle]
  (let [cube (fn [x] (* x x x))
        p (fn [x] (- ( * 3 x) (* 4 (cube x))))
        ]
    (if (not (> (Math/abs angle) 0.1))
      angle
      (do
        (println "a")
        (p (sine (/ angle 3.0)))))
    ))

; a = 5
; growth looks logarithmic due to the constant chopping of the input. We'll very quickly get to a small angle

; 1.16
;; Iterative fast exponentiation

;;iterable version of power is
;; given ax^y
;; 2^4 = 1(2)^4 = 2(2)^3 = 4(2)^2 = 8 * 2-

(defn fast-exp [a n]
  (cond
    (zero? n) 1
    (even? n) (Math/pow (fast-exp b (/ n 2)) 2)
    :else (* b (fast-exp b (dec n)))))

(defn fast-exp2 [a b n]
  (cond
    (zero? b) n
    (even? b) (fast-exp2 (* a a) (/ b 2) n)
    :else (fast-exp2 a (dec b) (* a n))))

; 1.17
; logarithmic multiplication

10 + 10 + 10 = 3 * 10 = 10 + (2 * 10)

(defn fast-mult [a n]
  (cond
    (zero? n) 0
    (even? n) (* 2 (fast-mult a (/ n 2)))
    :else (+ a (fast-mult a (dec n)))))

; 1.18
;; iterative logarithmic multipliation (Russian Peasant method)

(defn fast-mult2 [a n acc]
  (cond
    (zero? n) acc
    (even? n) (fast-mult (* 2 a) (/ n 2) acc)
    :else (fast-mult a (dec n) (+ acc a))))

; 1.19
;; a) show that
