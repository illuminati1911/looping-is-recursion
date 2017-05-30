(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (= 1 (count a-seq))
    (first a-seq)
    (if (= 0 (count a-seq))
      nil
      (recur (rest a-seq)))))

(defn seq= [seq1 seq2]
  (if (not (= (count seq1) (count seq2)))
    false
    (if (and (empty? seq1) (empty? seq2))
      true
      (if (= (first seq1) (first seq2))
        (recur (rest seq1) (rest seq2))
        false))))

(defn find-first-index [pred a-seq]
  (loop [acc 0 a-seq a-seq]
    (if (= 0 (count a-seq))
      nil
      (if (pred (first a-seq))
      acc
      (recur (+ acc 1) (rest a-seq))))))

(defn avg [a-seq]
  (loop [acc 1 sum 0]
    (if (> acc (count a-seq))
      (/ sum (- acc 1))
      (recur (+ acc 1) (+ sum (nth a-seq (- acc 1)))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [theset #{} index 0]
    (if (= index (count a-seq))
      theset
      (recur (toggle theset (nth a-seq index)) (+ index 1)))))

(defn fast-fibo [n]
  (loop [fibn 1 fibn-1 0 index 1]
    (if (< n 2)
      n
      (if (not (= index n))
        (recur (+ fibn fibn-1) fibn (+ 1 index))
        fibn))))

(defn cut-at-repetition [a-seq]
  (loop [theset #{} elems [] index 0]
    (if (= index (count a-seq))
      elems
      (if (contains? theset (nth a-seq index))
        elems
        (recur (conj theset (nth a-seq index)) (conj elems (nth a-seq index)) (+ index 1))))))
