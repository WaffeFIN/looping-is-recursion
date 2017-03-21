(ns looping-is-recursion)


(defn power [base exp]
 (let [helper (fn [acc base exp] (if (zero? exp)
                                  acc
                                  (recur (* acc base) base (dec exp))))]
  (helper 1 base exp
)))

(defn last-element [a-seq]
 (let [r (rest a-seq)]
  (if (empty? r) (first a-seq) (recur r)
)))

(defn seq= [seq1 seq2]
 (let [[e1 e2] [(first seq1) (first seq2)]]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (= e1 e2) (recur (rest seq1) (rest seq2))
   :else false
)))

(defn find-first-index [pred a-seq]
 (let [helper (fn [index pred a-seq] (cond
                                      (empty? a-seq) nil
                                      (pred (first a-seq)) index
                                      :else (recur (inc index) pred (rest a-seq))))]
  (helper 0 pred a-seq
)))

(defn avg [a-seq]
 (let [helper (fn [n sum a-seq] (if (empty? a-seq)
                                 (/ sum n)
                                 (recur (inc n) (+ sum (first a-seq)) (rest a-seq))))]
  (helper 0 0 a-seq
)))


(defn toggle [a-set elem]
 (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn parity [a-seq]
 (let [helper (fn [s a-seq] (if (empty? a-seq)
                                 s
                                 (recur (toggle s (first a-seq)) (rest a-seq))))]
  (helper #{} a-seq
)))


(defn fast-fibo [n]
 (let [helper (fn [pp p n] (if (zero? n)
                            pp
                            (recur p (+ pp p) (dec n))))]
  (helper 0 1 n
)))

(defn cut-at-repetition [a-seq]
 (let [helper (fn [s r-req a-seq] (if (empty? a-seq)
                                   r-req
                                   (let [e (first a-seq)]
                                    (if (contains? s e)
                                     r-req
                                     (recur (conj s e) (conj r-req e) (rest a-seq)
                                 )))))]
  (helper #{} [] a-seq
)))


