;; The balance example with atoms rather than vars or refs

(def balance (atom {}))
(defn deposit [amount]
  (swap! balance #(assoc % 'amt amount)))

(defn withdraw [amount]
  (let [currentBalance (@balance ['amt])]
    (if (>= currentBalance amount)
       (second (swap! balance #(assoc % 'amt (- currentBalance amount))))
       "Insufficient funds"
      )))

;; balance is global right now but may be hidden by creating a function that closes around some passed in
;; int value and returns a function that mutates it. This provides nice encapsulation & obviously prevents leaking
;; the balance atom into the rest of the code

;; 3.1
;; make an accumulator function (do is equivalent to Scheme's begin)
(defn make-accumulator [seed]
  (let [a (atom seed)
    accumulate (fn [x] (do
                         (swap! a #(+ % x))
                         @a))]
    accumulate))

(def A (make-accumulator 5))

;; 3.2
;; Write a call-counter taht records the number of times a particular function is called. This should close around
;; the argument function
(defn make-monitored [f]
  (let [a (atom 0)
        watch (fn [args] (cond
                           (= 'reset-count args) (reset! a 0)
                           (= 'how-many-calls? args) @a
                           :else (do
                                   (swap! a #(+ % 1))
                                   (f args))
                           ))
        ]
    watch
    ))

(def s (make-monitored #(printf %)))
(s "a")
(s "b")
(s 'how-many-calls?)                                        ;; 2
(s 'reset-count)                                            ;; 0
(s 'how-many-calls?)                                        ;; 0

;; 3.3
;; Update make-account to support password-protection
(defn make-account [balance password]
     (defn withdraw [amt]
       (if (>= @balance amt)
         (do (swap! balance #(- % amt))
             @balance
             )
         "Insufficient funds"
         ))
     (defn deposit [amt]
       (do  (swap! balance #(+ % amt))
            @balance
            ))
     (defn dispatch [m pw]
       (if (= pw password)
         (cond
           (= m 'withdraw) withdraw
           (= m 'deposit) deposit
           :else "Unknown request")
         "Bad Password"))
     dispatch )

(def acct (make-account (atom 100) 'yup))
((acct 'withdraw 'yup) 60)                                  ;;40
((acct 'withdraw 'yup) 10)                                  ;;30
((acct 'deposit 'yup) 100)                                  ;;130
((acct 'deposit 'nope) 100)                                  ;; Bad password (unable to apply arguments to string)



;; 3.4
;; add call the cops if the function is accessed more than 7 times
(defn make-account [balance password]
  (def failedPwAcc (atom 0))
  (defn call-the-cops [] (println "oh no!"))
  (defn withdraw [amt]
    (if (>= @balance amt)
      (do (swap! balance #(- % amt))
          @balance
          )
      "Insufficient funds"
      ))
  (defn deposit [amt]
    (do  (swap! balance #(+ % amt))
         @balance
         ))
  (defn dispatch [m pw]
    (if (= pw password)
      (do
        (swap! failedPwAcc #(+ 0 0))
        (cond
          (= m 'withdraw) withdraw
          (= m 'deposit) deposit
          :else "Unknown request")
        )
      (do (swap! failedPwAcc #(+ % 1))
          (if (= @failedPwAcc 7)
            (call-the-cops)
            "Bad Password"))
      ))
  dispatch )


;;3.5
;; Monte carlo integral estimation
;; NOTE this implementation isn't stack safe, so large trial values will cause overflows
(defn monte-carlo [trials experiment]
  (defn iter [remaining passed]
    (cond
      (zero? remaining) (/ passed trials)
      (experiment) (iter (dec remaining) (inc passed))
      :else (iter (dec remaining) passed)
      ))
  (iter trials 0)
  )

(defn rand-in-range [high low]
  (+  (rand-int (- high  low)) low))

(defn integration-experiment [x y r]
  (<= (* r r) (+
                (Math/pow (- (rand-in-range (- x r) (+ x r)) x) 2)
                (Math/pow (- (rand-in-range (- y r) (+ y r)) y) 2)
                )))

(defn monte-carlo-integration [trials]
  (defn experiment [] (integration-experiment 5 7 3))
   (* 10 (monte-carlo trials experiment))
  )

(monte-carlo-integration 5000)                              ;; returns something on the order of 1557/5000 ~~ 3.114

;; 3.6
;; Random number reset
;;apparently this (BSD forumla) is basically the worst LCG formula you can use
(defn linear-congruential-generator [seed]
  (mod (+  (* 1103515245 seed) 12345) (bit-shift-left 1 31))
  )

(defn random []
  (let [r (atom 959)]
    (defn go [message]
      (condp = message
        'reset #(reset! r %)
        'generate (do
                    (swap! r linear-congruential-generator)
                    @r)))
    go
    ))

;;3.7
;; make joint bank account
(defn make-account [balance password]
  (defn withdraw [amt]
    (if (>= @balance amt)
      (do (swap! balance #(- % amt))
          @balance
          )
      "Insufficient funds"
      ))
  (defn deposit [amt]
    (do  (swap! balance #(+ % amt))
         @balance
         ))
  (defn dispatch [m pw]
    (if (= pw password)
      (cond
        (= m 'withdraw) withdraw
        (= m 'deposit) deposit
        :else "Unknown request")
      "Bad Password"))
  dispatch )

(defn make-joint [account password newPassword]
  (let [f (fn [cmd]
               (account cmd password))]
    #(if (= newPassword %2)
       (f %)
       "Bad Password!")
    )
  )

(def acct (make-account (atom 100) "guest"))
(def myAcct (make-joint acct "guest" "baboon"))
((myAcct 'withdraw "baboon") 50) ; 50

;;3.8
;; Implement a simple function that changes the return value based on the evaluation order within addition
(def state (atom []))
(defn f [x]
  (if (empty? @state)
    (do (swap! state conj x)
      x)
    (min (first @state) x)
    )
  )

(def left (+ (f 0) (f 1)))
(reset! state [])
(def fakeRight (+ (f 1) (f 0)))

;; 3.9
;; draw env models for the following code

(defn fact [n]
  (if (= n 1)
    1
    (* n (fact (dec n))))
  )
;;
;; ===========================
;;|                           |
;;|fact:                      |
;;=============================
;;
;; =========
;; |       | <----
;; |n: int |     | loops back to itself, creating a new identical environment referencing this one
;; ========= ----|
;; E1
;;

;; 3.10
;; illustrate the difference between using a local state variable for lambdas vs. a closure over the parameter

;; I'll be answering these in long form rather than ascii art from now on. Art is hard.
;; The stacking/chaining of environments into what's effectively a linked-list means that whether we create a local var
;; in the let example or close over the parameter, the variable will be resolved by walking up the environment chain
;; from the returned lambda & finding the value of "balance" defined there. This is why they are equivalent (provided there's no shadowing)

;; 3.11
;;more boxes and pointers

;; 3.12
;; what happens in the two appends

;;3.13
;;make cycle. what happens if you try to get the last element of a cycle?
;; infinite loop. It's a cycle...

;;3.14
;; what does mystery function do?
;; this reverses a list

;;3.15
;;explain what happens in the following function
(def x (list 'a 'b))
(def z1 (cons x x))
;;(('a 'b) 'a 'b)

(def z2 (cons (list 'a 'b) (list 'a 'b)))
;;(('a 'b) 'a 'b)

;; now mutate the head of the list
;; in the first example, because x is shared in the hosting environment, a change to x changes the head of the first list
;; which in turn changes the head of the cdr as well. alternatively, the z2 list has no shared elements, so mutation won't change it

;;3.16
;; find counter points that prove this function wrong
(defn pair? [x]
  (and (coll? x) (>= (count x) 2)))

(defn count-pairs [x]
  (if (not (pair? x))
    0
    (+ (count-pairs (first x))
       (count-pairs (second x))
       1)
    ))


(count-pairs '(1 (2 (3 4)))) ;; 3
(def a [1 2])
(def b [a 4])
(def c [a b])
(count-pairs [a [b c]]) ;; 7

(def a [1 2])
(def b [a a])
(def lst [a b])
(def looping-list [b lst])
(count-pairs [a b])

;;3.17
;; correct count-pairs implementation
(require clojure.set)

(defn count-pairs [p]
  (let [seen (atom '())]
    (defn iter [x]
      (if (or (not (pair? x)) (not (nil? (some #{x} @seen))))
        0
        (do
          (swap! seen conj x)
          (+ (iter (first x))
             (iter (second x))
             1)
          )
        )
      )
    (iter p)
    ))

;;3.18
;; find the cycle in a list

(defn cyclic? [ls]
  (defn iter [seen xs]
    (cond
      (not (pair? xs)) false
      (= (rest xs) xs) true
      (= (first xs) rest xs) (iter seen (rest xs))
      (not (nil? (some #{first xs} seen))) true
      (not (nil? (some #{rest xs} seen))) true
      :else
        (if (not (pair? (first xs)))
          (iter (cons xs seen) (rest xs))
          (or
            (iter (cons xs seen) (first xs))
            (iter (cons xs seen) (rest xs)))
          )
      )
    )
  (iter '() ls)
  )

;;3.19
;;find the cycle efficiently
(defn efficient-cyclic? [ls]
  (defn iter [tortise hare]
    (if (= (first tortise) (first hare))
      true
      (iter (rest tortise) (rest (rest hare)))
      )
    )
  (iter ls (rest ls))
  )


