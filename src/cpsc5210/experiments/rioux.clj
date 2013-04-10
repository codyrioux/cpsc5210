(ns cpsc5210.experiments.rioux
  ""
 (:use (cpsc5210 rioux)
       (cpsc5210 util)) )

(defn initial-population
  [cnt lines max-gates]
  (repeatedly cnt #(random-circuit lines max-gates)))

;; 4 mod 5

(def circuit-4mod5
  [[[:a] :c]
   [[:b] :d]
   [[:c :d] :e]
   [[:c] :d]
   [[:d] :e]])

(def truth-4mod5 (tofolli-to-truth-table circuit-4mod5))

(defn terminate-4mod5?
  "A termination condition that terminates the moment a member of the population
   satisfies the circuit."
  [population]
  (> 0 (count (filter #(= truth-4mod5 (tofolli-to-truth-table %1)) population))))

;; NEED crossover,
