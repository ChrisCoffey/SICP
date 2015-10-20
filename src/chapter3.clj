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