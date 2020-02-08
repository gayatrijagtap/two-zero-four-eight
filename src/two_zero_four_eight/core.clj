(ns two-zero-four-eight.core)

(defn not-zero? [e] (not (zero? e)))

(defn append-zeros [row]
  (concat row (repeat (- 4 (count row)) 0)))

(defn partition-identical-pair [row]
  (apply concat (map (partial partition 2 2 nil)
                     (partition-by identity row))))

(defn move-left [x]
  (->> x
       partition-identical-pair
       (map (partial reduce +))
       (filter not-zero?)
       (append-zeros)))
