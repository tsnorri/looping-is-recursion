(ns looping-is-recursion)


(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))


(defn power [base exp]
  (let [helper (fn [res base exp]
                (if (zero? exp)
                  res
                  (recur (* res base) base (dec exp))))]
    (helper 1 base exp)))


(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))


(defn seq= [seq1 seq2]
  (let [r1 (rest seq1)
        r2 (rest seq2)]
      (if (= (first seq1) (first seq2))
        (if (and (empty? r1) (empty? r2))
          true
          (recur r1 r2)
          )
        false)))


(defn find-first-index [pred? a-seq]
  (loop [r a-seq
         i 0]
    (if (empty? r)
      nil
      (if (pred? (first r))
        i
        (recur (rest r) (inc i))))))


(defn avg [a-seq]
  (loop [r a-seq
         s 0
         i 0]
    (if (empty? r)
      (/ s i)
      (recur (rest r) (+ s (first r)) (inc i)))))


(defn parity [a-seq]
  (loop [r a-seq
         p #{}]
    (if (empty? r)
      p
      (recur (rest r) (toggle p (first r))))))


(defn fast-fibo [pn]
  (loop [a 0
         b 1
         n pn]
    (if (zero? n)
      a
      (recur b (+ a b) (dec n)))))


(defn cut-at-repetition [a-seq]
  (loop [r a-seq
         p #{}
         s []]
    (if (empty? r)
      s
      (if (contains? p (first r))
        s
        (recur (rest r) (toggle p (first r)) (conj s (first r)))))))
