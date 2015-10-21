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