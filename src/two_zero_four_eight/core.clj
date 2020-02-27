(ns two-zero-four-eight.core)

(defn not-zero? [e] (not (zero? e)))

(def transpose (partial apply mapv vector))

(defn append-zeros [row]
  (concat row (repeat (- 4 (count row)) 0)))

(defn partition-identical-pair [row]
  (apply concat (map (partial partition 2 2 nil)
                     (partition-by identity row))))

(defn evaluate [row]
  (->> row
       (filter not-zero?)
       partition-identical-pair
       (map (partial reduce +))))

(defn move-left [grid]
  (map (comp append-zeros evaluate) grid))

(defn move-right [grid]
  (map (comp reverse append-zeros evaluate reverse) grid))

(defn move-up [grid]
  (transpose (move-left (transpose grid))))

(defn move-down [grid]
  (transpose (move-right (transpose grid))))

(defn get-initial-grid
  (mapv (partial repeat 4) (repeat 4 0)))

(defn get-zero-indexes [coll]
  (keep-indexed (fn [x y] (when (zero? y) x)) coll))

(defn insert-random-twos [grid n]

  )

(defn start-game
  (loop [grid (get-initial-grid)]
    )
  )