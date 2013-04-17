(ns cpsc5210.experiments.rioux
  ""
 (:use (cpsc5210 rioux)
       (cpsc5210 util)
       (cpsc5210 core))
  (:gen-class))

(defn initial-population
  [cnt lines max-gates]
  (repeatedly cnt #(random-circuit lines max-gates)))

(defn terminate?
  [target population]
  (>(count (filter #(= target (tofolli-to-truth-table %1)) population)) 0))

(defn parallel-experiment
  [experiment cnt]
  (pmap #(experiment %1) (range cnt)))

;; 4 mod 5

(def circuit-4mod5
  [[[:a] :c]
   [[:b] :d]
   [[:c :d] :e]
   [[:c] :d]
   [[:d] :e]])

(def truth-4mod5 (tofolli-to-truth-table circuit-4mod5))

(defn experiment-4mod5
  ""
  [x]
  (let [ip0 (initial-population 500 [:a :b :c :d :e] 10)]
    (ga (partial fitness3 circuit-4mod5) selection crossover (partial mutation [:a :b :c :d :e]) (partial terminate? truth-4mod5) ip0 300 0.05)))

;; xor5

(def circuit-xor5
  [[[:a] :b]
   [[:b] :c]
   [[:c] :d]
   [[:d] :e]])

(def truth-xor5 (tofolli-to-truth-table circuit-xor5))

(defn experiment-xor5
  ""
  [x]
  (let [ip0 (initial-population 500 [:a :b :c :d :e] 10)]
    (ga (partial fitness3 circuit-xor5) selection crossover (partial mutation [:a :b :c :d :e]) (partial terminate? truth-xor5) ip0 300 0.05)))

;; prime3

(def circuit-prime3
  [[[:a] :c]
   [[:c] :b]
   [[:a :b] :c]
   [[:b] :a]])

(def truth-prime3 (tofolli-to-truth-table circuit-prime3))

(defn experiment-prime3
  ""
  [x]
  (let [ip0 (initial-population 100 [:a :b :c] 5)]
    (ga (partial fitness3 circuit-prime3) selection crossover (partial mutation [:a :b :c]) (partial terminate? truth-prime3) ip0 150 0.05)))



;; easy


(def circuit-easy
  [ [[:a :b] :c]])

(def truth-easy (tofolli-to-truth-table circuit-easy))

(defn experiment-easy
  ""
  [x]
  (let [ip0 (concat [ ] (initial-population 10 [:a :b :c] 2))]
    (ga (partial fitness3 circuit-easy) selection crossover (partial mutation [:a :b :c]) (partial terminate? truth-easy) ip0 10 0)))

;; WORST CODE DUPLICATION EVER, WELL NOT EVER
(defn -main
  [experiment cnt id]
  (cond
    (= experiment "4mod5")
    (spit (str "4mod5-results-" id) (vec (parallel-experiment experiment-4mod5 (read-string cnt)))) 
    (= experiment "xor5")
    (spit (str "xor5-results-" id) (vec (parallel-experiment experiment-xor5 (read-string cnt)))) 
    (= experiment "prime3")
    (spit (str "prime3-results-" id) (vec (parallel-experiment experiment-prime3 (read-string cnt))))
    (= experiment "easy")
    (spit (str "easy-results-" id) (vec (parallel-experiment experiment-easy (read-string cnt))))))

;; Reults Aggregation

(defn read-results
  [experiment ids]
  (apply concat (for [id ids]
                  (read-string (slurp (str "target/run1/" experiment "-results-" id)))
                  ))
  )

(defn get-solutions
  [target results]
  (let [solutions (apply concat (map second results))
        solutions (filter #(= target (tofolli-to-truth-table %1)) solutions)]
    solutions))
