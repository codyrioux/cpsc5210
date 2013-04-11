(ns cpsc5210.experiments.rioux
  ""
 (:use (cpsc5210 rioux)
       (cpsc5210 util)
       (cpsc5210 core)))

(defn initial-population
  [cnt lines max-gates]
  (repeatedly cnt #(random-circuit lines max-gates)))

(defn terminate?
  [target population]
  (> 0 (count (filter #(= target (tofolli-to-truth-table %1)) population))) )

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
  []
  (let [ip0 (initial-population 1000 [:a :b :c :d :e] 10)]
    (ga (partial fitness3 circuit-4mod5) selection crossover (partial mutation [:a :b :c :d :e]) (partial terminate? truth-4mod5) ip0 100 0.1)))

;; xor5

(def circuit-xor5
  [[[:a] :b]
   [[:b] :c]
   [[:c] :d]
   [[:d] :e]])

(def truth-xor5 (tofolli-to-truth-table circuit-xor5))

(defn experiment-xor5
  ""
  []
  (let [ip0 (initial-population 1000 [:a :b :c :d :e] 10)]
    (ga (partial fitness circuit-xor5) selection crossover (partial mutation [:a :b :c :d :e]) (partial terminate? truth-xor5) ip0 100 0.1)))
