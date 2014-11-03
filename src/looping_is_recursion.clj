(ns looping-is-recursion)

(defn power [base exp]
  (let [calculation (fn [base exp sum]
                        (if (== exp 0)
                         sum
                         (recur base (dec exp) (* base sum))
                      ))]
    (calculation base exp 1)
  ))



(defn last-element [a-seq]
  (let [findLast (fn [a-seq l]
                      (cond
                        (empty? a-seq)             nil
                        (== l (count a-seq))       (first a-seq)
                        :else                      (recur (rest a-seq) l)
                      ))]
    (findLast a-seq 1)
  ))



(defn seq= [seq1 seq2]
  (let [isEqual (fn [seq1 seq2 same]
                (cond
                (and (empty? seq1) (empty? seq2)) true
                same                              (recur (rest seq1) (rest seq2) (== (first seq1) (first seq2)))
                :else                             false
        ))]
        (isEqual seq1 seq2 (== (count seq1) (count seq2)))
  ))



(defn find-first-index [pred a-seq]
  (let [findIndex (fn [n a-seq condition]
                   (cond
                   (empty? a-seq) nil
                   (pred (first a-seq)) n
                   :else (recur (inc n) (rest a-seq) condition)
                    ))]
    (findIndex 0 a-seq pred)
    ))


(defn avg [a-seq]
  (loop [acc 0 index 0 maxBound (count a-seq)]
    (cond
    (== maxBound 0) 0
    (== maxBound 1) (first a-seq)
    (== index maxBound)         (/ acc index)
    :else                       (recur (+ acc (get a-seq index)) (inc index) maxBound)
  )))


(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else                  (conj a-set elem)
  ))

(defn howManyTimes[a-seq elem]
  (loop [n 0 index 0 maxBound (count a-seq)]
  (cond
  (= index maxBound)         n
  (= (get a-seq index) elem) (recur (inc n) (inc index) maxBound)
  :else                      (recur n (inc index) maxBound)
  )))

(defn parity [a-seq]
   (let [doStuff (fn [a-seq setted index newSet]
                     (cond
                     (empty? setted)                                    newSet
                     (== index (count (set a-seq)))                     newSet
                     (== (mod (howManyTimes a-seq (first setted)) 2) 0) (recur a-seq (rest setted) (inc index) newSet)
                     :else                                              (recur a-seq (rest setted) (inc index) (conj newSet (first setted)))
                   ))]
     (doStuff a-seq (set a-seq) 0 #{})
  ))


(defn fast-fibo [n]
  (loop [f 1 prev 0 looped 2]
    (cond
    (== n 0) prev
    (== n 1) f
    (== looped n) (+ f prev)
    :else         (recur (+ f prev) f (inc looped))
  )))



(defn cut-at-repetition [a-seq]
  (let [findRepetition (fn [old new]
                       (cond
                       (empty? old)                                new
                       (contains? (set new) (first old))           new
                       :else                                       (recur (rest old) (conj new (first old)))
                        ))]
    (findRepetition a-seq [])
  ))



