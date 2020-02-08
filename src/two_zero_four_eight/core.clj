(ns two-zero-four-eight.core)

(defn not-zero? [e] (not (zero? e)))

(defn append-zeros [row]
  (concat row (repeat (- 4 (count row)) 0)))

(defn move-left [x]
  (append-zeros
    (filter not-zero?
            (map (partial reduce +)
                 (apply concat (map (partial partition 2 2 nil)
                                    (partition-by identity x)))))))

