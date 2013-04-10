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
  [population]
  )

;; NEED fitness, selection, crossover,
