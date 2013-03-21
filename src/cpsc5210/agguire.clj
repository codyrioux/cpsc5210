(ns cpsc5210.agguire
  "An incomplete implementation of the experiments outlined in
   the Agguire 2003 paper on evolutionary logic synthesis using
   entropy based fitness functions.
   
   This implementation lacks a fitness function, and crossover
   is incomplete."
  (:use (cpsc5210 util))
  (:require [clojure.zip :as z]))

(defn random-node [x] (loop [cnt (rand-int (count (flatten x))) node x]
                        (cond (= cnt 0) (z/node node)
                              :else (recur (- cnt 1) (z/next node))) ))

(def test-circuit [:b [:c [:a 0 1] [:a 1 0]] [:c [:a 1 0] 0]])

(defn selection
  "Select which individuals advance based on a probability distribution defined by their fitness."
  [individuals]
  (let [max-fitness (max (map #(:fitness (meta %1)) individuals))]
    (filter #(< (/ (:fitness (meta %1)) max-fitness ) (rand)) individuals)))

(defn crossover
 "Randomly exchange two nodes in each a pair of elements.
  TODO: Actually crossover."
 [x y]
  (let [crossover-point-x (random-node x)
        crossover-point-y (random-node y)])
  )

(defn mutation
  "Randomly mutate the passed element based on the mutation rate.
   A leaf (0 or 1 input) is fliped and a mux has its input selected
   randomly out of the list. Note it is possible for a mux to mutate to itself."
  [mutation-rate x]
  (if
    (< mutation-rate (rand))
    (let [node (random-node x)
          inputs (seq (set (filter keyword? (flatten (z/node x))))) ]
      (if (z/branch? node) (z/root (z/replace node [(rand-nth inputs) (second node) (last node)])) 
        (if (number? z/node node) (z/root (z/replace node (Math/abs (- node 1))))
          (z/root node))))
    x))
