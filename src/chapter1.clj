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

(defn improve [guess x]
  (average guess (/ x guess)))

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

(defn fast-exp [b n]
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
;; finish this implemenation of fast fibonacci, such that a = bq + aq + ap && b = bp + aq

(defn fib [n]
  (let [fib-iter (fn iter [a b p q count]
                   (cond
                     (zero? count) b
                     (even? count) (iter
                                     a
                                     b
                                     (+ (* p p ) (* q q))
                                     (+ (* 2 p q) (* q q))
                                     (/ count 2))
                     :else (iter
                             (+ (* b q) (* a q) (* a p))
                             (+ (* b p) (* a q))
                             p
                             q
                             (- count 1))))]
    (fib-iter 1 0 0 1 n)
    ))

;; Euler's algorithm
(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (unchecked-remainder-int a b))))

; 1.20
;; Using Lame's theorem, illustrate the gcd process under normal order vs applicative order for gcd(206, 40)

;; Lame's theorm says that if n is the kth fibonacci number and m is the smaller number in Euler's algoritm after k steps,
;; then n >= m.
;;
;; Under applicative order evaluation, this call stack would look like this
;; gcd(206, 40) -> gcd(40, 6) -> gcd(6, 4) -> gcd(4, 2) -> gcd(2, 0) -> 2
;;
;; this means there are 4 remainder evaluations for the given problem. If we change this to normal order, because the
;; exression (remainder a b) replaces b, there end up being 18 different calls to remainder. Basically normal order
;; never caches the result of an expression, instead evaluating it at ever point. Applicative order will eagerly evaluate
;; then cache the value for use wherever the symbol is used.

;; Fermat's Little Theorem
;; If n is a prime and a is a positive number < n, then a^n is congruent to a modulo n

(defn expmod [base exp m]
  (cond
    (zero? exp) 1
    (even? exp)
      (unchecked-remainder-int (Math/pow (expmod base (/ exp 2) m) 2) m)
    :else
      (unchecked-remainder-int (* base (expmod base (dec exp) m)) m)))

(defn fermatTest [n]
  (let [iter (fn it [n count]
               (let [a (inc (rand-int (dec n)))]
                 (cond
                   (zero? count) true
                   (= (unchecked-remainder-int a n) (expmod a n n)) (it n (dec count))
                   :else false
                   )
                 ))
        ]
    (iter n 10)))

; 1.21
;; find the smallest divisor for 199, 1999, and 19999

(defn smallestDivisor [n]
  (let [iter (fn it [i div]
     (cond
       (>= i (Math/sqrt n)) div
       (zero? (unchecked-remainder-int n i)) (it (inc i) (/ n i))
       :else (it (inc i) div)
       ))
      ]
  (iter 2 0)))

; 1.22
;; timed prime test


(defn slowPrime? [n]
  (if(zero? (smallestDivisor n))
    true
    false))

(defn report-prime [timeSpent n]
  (println " *** ")
  (println n)
  (println timeSpent))

(defn timed-prime-test [n]
  (newline)
  (start-prime-test n (System/currentTimeMillis)))

(defn start-prime-test [n start-time]
  (if (slowPrime? n) (report-prime (- (System/currentTimeMillis) start-time) n)))

(defn checkPrimesInRange [ls]
  (if (seq ls)
    (do
      (timed-prime-test (first ls))
      (checkPrimesInRange (rest ls)))))

(defn oneK []
  (->>
    (range 1000 1100)
    (filter odd?)
    ))

(defn tenK []
  (->>
    (range 10000 10100)
    (filter odd?)
    ))

(defn hundredK []
  (->>
    (range 100000 100100)
    (filter odd?)
    ))


; 1.23
;; rewrite slow primes to only check odds if the number is odd
(defn smallestDivisor [n]
  (let [iter (fn it [i div]
               (cond
                 (>= i (Math/sqrt n)) div
                 (zero? (unchecked-remainder-int n i)) (it (+ i 2) (/ n i))
                 :else (it (+ i 2) div)
                 ))
        ]
    (if (zero? (unchecked-remainder-int n 2)
          2
          (iter 3 0)
      ))))

; 1.24
;; modify the timed primes code to use fast primes. Honestly this is an exercise in futility since my machine walks through
;; the slow primes example in ~2ms for the slowest case

; 1.25
; is the fast and simple exponential equivalent to expmod
(defn simple-exp-mod [base exp m]
  (unchecked-remainder-int (fast-exp2 base exp m) m))

;; I believe this should work and be eqivalent to the current expmod, but becase it isn't optimized with successive squaring to reduce
;; the size of numbers, it will not perform as well. Bill the Lizzard backed this up in his explanation

; 1.26
;; converting expmod's square call to multiplication results in an O(n) process rather than O(log n)

;; This ocurs because both sides of the multiplication are expanded out, whereas the first impleentation using
;; exponentiation only builds out a single path, rendering it a normal linear recursive process. Having > 1 path leads to
;; a tree recursive process, which means we're going to do significantly more work.

;1.27
;; Check the Carmichael numbers. To run this, just pass in the Fermat test. I've tweaked it to make 1.28 easier

(defn carmichaels [f]
  (let [nums [561, 1105, 1729, 2465, 2821, 6601, 8911]]
    (println (map f nums))
    ))

;1.28
; Miller-Rabin test
;; This is a modified version of the Fermat test which cannot be tricked by the Carmichael numbers. It stats that
;; for any a < , a^(n-1) is congruent to 1 modulo n

(defn expmod2 [base exp m]
  (cond
    (zero? exp) 1
    (even? exp)
    (let [f (fn [x y]
              (if
                (and (not (or (= x 1) (= x (dec m)))) (= (rem (* x x) m) 1))
              0
              (rem (* x x) m)
              ))]
      (f (expmod2 base (/ exp 2) m ) m))
    :else
    (unchecked-remainder-int (* base (expmod2 base (dec exp) m)) m)))

(defn miller-rabin [n]
  (let [iter (fn it [n count]
               (let [a (inc (rand-int (dec n)))]
                 (cond
                   (zero? count) true
                   (= (unchecked-remainder-int a n) (expmod2 a n n)) (it n (dec count))
                   :else false
                   )
                 ))
        ]
    (iter n 10)))

;; shockingly this works fine. The let function in the middle of expmod2 says that if x is not 1 or equal to 1 less tha m and (x^2)/m has remainder 1

; Summations and higher order procedures
(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a) (sum term (next a ) next b))
 ))

(defn integral [f a b dx]
  (let [add-dx (fn [x]
                 (+ x dx))
        ]
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)
    ))

; 1.29
;; Simpson's rule is a more accurate method of calculating integrals than the summation of x + dx + dx/2 ...
(defn simpson-integral [f a b n]
  (let [h (/ (- b a) n)
        y (fn [x] (f(+ a (* x h))))
        c (fn [x] (if
                    (even? x) ( * 2 (y x))
                    (* 4 (y x))))]
    (* (/ h 3) (sum c 0 inc n))))

;1.30
;; Simpson's rule with iterative recursion

(defn sum [term a next b]
  (let [f (fn iter [a result]
            (if (> a b)
              result
              (iter (next a) (+ (term a) result))))
            ]
    (f a 0)))

; 1.31
;; a. higher order product recursive
(defn product [term a next b]
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))
    ))

;; b. higher order product iterative
(defn product [term a next b]
  (let [f (fn iter [a result]
            (if (> a b)
              result
              (iter (next a) (* (term a) result))))
        ]
    (f a 1)
    ))

; 1.32
;; sum and product are both special cases of accumulate
;a
 (defn accumulate [combiner identity term a next b]
  (if (> a b)
    identity
    (combiner (term a) (accumulate combiner identity term (next a) next b))
    ))

;b
(defn accumulate [combiner identity term a next b]
  (let [f (fn iter [a result]
            (if (> a b)
             result
              (iter (next a) (combiner (term a) result))))
        ]
    (f a identity)
    ))

; 1.33
;; filtered accumulate. Apparently this is
(defn filter-accumulate [pred combiner identity term a next b]
  (let [f (fn iter [a result]
            (if (> a b)
              result
              (iter (next a) (if (pred a)
                               (combiner (term a) result)
                               result
                               ))))
        (f a identity)
        ]))

;a sum of primes squares
(defn sumOfPrimes [a b]
  (filter-accumulate (fn [x] (miller-rabin x)) + 0 (fn [x] (* x x)) a inc b))

;b product of relative primes
(defn relativePrimesLessThan [n]
  (filter-accumulate (fn [x] (= (gcd x n) 1)) * 1 (fn [x] (x)) 1 inc n))

; 1.34
;; interpret (f f)
(defn f [g]
  (g 2))

;; Applying f to itself should result in a stack overflow as the function call recurses infinitely


;; note
;; a zero is a case where, for a given function f, f(x) = 0
;; a fixed point of a function f is a case where f(x) = x
;;average damping is the process of averaging successive solutions to converge on a result
;;; very useful in converging on fixed-points


;1.35
;; show that the golden ratio is a fixed point of the transformation x |-> 1 + (1/x)
(defn fixed-point [f initialGuess]
  (let [ tolerance 0.00001
         close-enough? (fn [a b] (< (Math/abs (- a b)) tolerance))
         try-it (fn t [guess]
               (let [next (f guess)]
                 (if (close-enough? guess next)
                   next
                   (t next))
                 ))
         ]
    (try-it initialGuess)
    ))
;; note to self don't name things try, it's a protected word from java for try/catch (doh!)

(fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0)


; 1.36
;; log the steps in fixed-point
(defn log-fixed-point [f initialGuess]
  (let [ tolerance 0.00001
         close-enough? (fn [a b] (< (Math/abs (- a b)) tolerance))
         try-it (fn t [guess]
                  (let [next (f guess)]
                    (if (close-enough? guess next)
                      next
                      (do
                        (println next)
                        (t next)))
                    ))
         ]
    (try-it initialGuess)
    ))

(log-fixed-point (fn [x] (/ (Math/log 1000) (Math/log x))) 2.0)

(defn log-damp-fixed-point [f initialGuess]
  (let [ tolerance 0.00001
         close-enough? (fn [a b]
                         (< (Math/abs (- a b)) tolerance))
         average-damping (fn [x y]
                           (/ (+ y x) 2))
         try-it (fn t [guess]
                  (let [next (f guess)]
                    (if (close-enough? guess next)
                      next
                      (do
                        (println next)
                        (t (average-damping next guess)))
                    )))
         ]
    (try-it initialGuess)
    ))

(log-damp-fixed-point (fn [x] (/ (Math/log 1000) (Math/log x))) 2.0)

;; average damping significantly reduces the number of steps to reach the target. Very cool

; 1.37
;; k-term finite continued fraction. terminate an infinite continued fraction expansion after some number k terms

(defn cont-frac [n d k]
  (let [f (fn iter [a]
            (if (= a k)
              (/ (n a) (d a))
              (/ (n a) (+ (d a) (iter (inc a))))
              ))
        ]
    (f 0))
  )

(cont-frac (fn [x] 1.0) (fn [x] 1.0) 10)
;; 10 iterations is roughly enough to find 1/Ф out to 4 decimal places

; 1.38
;; Euler's e-2 continued fraction
(cont-frac (fn [x] 1.0) (fn [x]
                          (if (= (rem x 3) 2)
                            (* (int (/ (inc x) 3)) 2)
                            1))
  10)

; 1.39
;; tangent continued fraction
(defn tan-cf [x k]
  (let [ n (fn [y] (if (= 1 y) x (* x x)))
         d (fn [x] (+ x (dec x)))
         f (fn iter [a]
            (if (= a k)
              (/ (n a) (d a))
              (/ (n a) (- (d a) (iter (inc a))))
              ))
        ]
    (f 1)
    ))

;; I'm not sure about the formula above

(defn average [x y] (/ (+ x y) 2))

(defn avg-damp [f]
  (fn [x] (average x (f x))))

(defn sqrt [x]
  (fixed-point
    (avg-damp (fn [y] (/ x y)))
    1.0)
  )

(defn cube-root [x]
  (fixed-point
    (avg-damp (fn [y] (/ x (* y y))))
    1.0
    ))


;; If x -> g(x) is differentiable, then the zero of g(x) (g(x) = 0) is a fixed point of x -> f(x) where
;; f(x) = x - g(x)/ D(g(x))

(def dx 0.00001)
(defn deriv [g]
  (fn [x]
    (/
      (- (g (+ x dx)) (g x))
      dx)
    ))

(defn netwton-transform [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))

(defn netwon-method [g guess]
  (fixed-point (netwton-transform g) guess))


; 1.40
;; cubics
(defn cubic [a b c]
  (fn [x] (+ (* x x x) (* a (* x x)) (* b x) c)))

(netwon-method (cubic 1 2 3) 1)
;; Pretty mechanical transformation. But it does find the fixed point.


; 1.41
;; double application
(defn doubleApply [f] (comp f f))
(defn double-other [f]
  (fn [x] (f (f x))))

; 1.42
;; write compose
(defn compose [f g]
  (fn [x] (f (g x))))

; 1.43
;; repeated application
(defn repeated [f x]
  (fn [a]
    (let [r (fn rec [i y]
              (if (zero? i)
                y
                (rec (dec i) (f y))))
          ]
      (r x a)
      ))
  )


;1.44
;; smoothing function
(defn smooth [f dx]
  (fn [x] (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3)))

(defn n-fold-smooth [f dx n]
  (repeated ((smooth f dx) n)))

; 1.45
;; compute nth roots using average damping

(defn nth-root [n]
  (fixed-point
    (repeated avg-damp (fn [x] (/ n (Math/pow x (dec n))))) 1.0))

;; note this is an incomplete solution

; 1.46
;; iterative improvement function
(defn iterative-imporove [f p]
  (fn try-do [guess]
    (if (p guess)
      guess
      (try-do (f guess))))
  )

(defn sqrt [x]
  ((iterative-improve
     (fn [guess] (good-enough? guess x))
     (fn [guess] (improve guess x))) 1.0)
  )


(defn fixed-point [f g]
  ((iterative-improve
     (fn [guess] (good-enough? guess (f guess)))
     f)
   g)
  )