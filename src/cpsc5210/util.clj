(ns cpsc5210.util
  "A collection of utility functions for use in these experiments."
  (:require [clojure.zip :as z]))

(defn binary-permutation
  "Takes a seq of input keywords and maps them to all possible binary permutations.
   This is useful for constructing truth tables for all possible inputs."
  [inputs]
  (loop [k inputs, accumulator [{}]]
    (cond 
      (empty? k) accumulator
      :else (recur (rest k)
                   (concat 
                     (map #(assoc %1 (first k) 0) accumulator)
                     (map #(assoc %1 (first k) 1) accumulator)))) ))

(defn flip-bit
  "Flips a bit."
  [bit]
  (Math/abs (- bit 1)))

(defn evaluate-tofolli-gate
  "Evaluates a single tofolli gate.
   Inputs:
   values : A map containing inputs and their binary value.
   gate : A representation of a tofolli gate. ([:a :b] :c) a, b are controls and c the target.
   
   Returns: A map in the same form as values representing the gate output."
  [values gate] 
  (if (every? #(= 1 (get values %1)) (first gate))
    (assoc values (second gate) (flip-bit (get values (second gate))) )
    values))

(defn evaluate-tofolli-circuit
  "Evaluates an entire tofolli circuit."
  [values circuit]
  (reduce evaluate-tofolli-gate values circuit))

(defn tofolli-to-truth-table
  "Builds a truth table for the provided tofolli cricut."
  [circuit]
  (let [inputs (set (filter keyword? (flatten circuit)))
        input-patterns (binary-permutation inputs)]
    (reduce #(assoc %1 %2 (evaluate-tofolli-circuit %2 circuit)) {} input-patterns)))

(defn quantum-cost-gate
  "Computes the quantum cost of a single gate.
   Maximum five symbols."
  [gate]
  (let [size-cost-map {1 1, 2 1, 3 5, 4 13, 5 29}]
    (get size-cost-map (count (flatten gate))))) 

(defn circuit-quantum-cost
  "Computes the quantum cost of an entire circuit."
  [circuit]
  (reduce + (map quantum-cost-gate circuit)))
