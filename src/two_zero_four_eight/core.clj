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

(defn get-zero-indexes [coll]
      (keep-indexed (fn [x y] (when (zero? y) x)) coll))


(defn flat-grid [grid]
      (vec (flatten grid)))


(defn add-random-twos [grid]
      (let [flatten-grid (flat-grid grid) zero-indexes (get-zero-indexes flatten-grid)]
           (if (empty? zero-indexes) -1 (assoc flatten-grid (rand-nth zero-indexes) 2))))


(def initial-grid
  (nth (iterate add-random-twos (mapv (partial repeat 4) (repeat 4 0))) 2))


(defn print-grid
      [grid]
      (->> grid
           (partition 4)
           (map (fn [x] (clojure.string/join "|" x)))
           (clojure.string/join "\n")
           (println)
           ))


(defn update-grid [grid instruction] (case instruction
                             "l" (move-left grid)
                             "r" (move-right grid)
                             "u" (move-up grid)
                             "d" (move-down grid)
                             (move-left grid)))


(defn start-game []
      (loop [grid initial-grid]
            (print-grid grid)
            (println "Please enter move (up:u, down:d, left:l, right:r):")
            (let [instruction (read-line) updated-grid (flat-grid (update-grid (partition 4 grid) instruction))]
                 (print-grid updated-grid)
                 (println "-------------------")
                 (if (empty? (get-zero-indexes updated-grid))
                   (print "Game over!!")
                   (recur (add-random-twos updated-grid))
                   )
                 )
            )
      )