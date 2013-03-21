(ns cpsc5210.rioux
  (:use (cpsc5210 util)
        (clojure set)))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn- p 
  "Calculates the probability of a particular output given a circuit.
   Currently cheats and assumes we have a reversible circuit, thus the output
   occurs exactly once."
  [circuit output]
  (/ 1  (count (set (filter keyword? (flatten circuit))))))

(defn- p2
  "Determines the probability of seeing r=rval | t=tval"
  [ttable rtable tkey tval rkey rval]
  (let [t-outputs (map #(get ttable %1) (keys ttable))
        t-frac (/ (count (filter #(= tval (get toutputs tkey)) toutputs)) (count toutputs))
        t-inputs-given-output (filter #(= (get (get ttable %1) tkey) tval)  (keys ttable))
        r-outputs (map (get rtable %1) t-inputs-given-output) 
        r-frac (/ () (count r-outputs))
        ]
    (* t-frac r-frac)))

(defn h
  "Calculates the entropy of a given boolean function specified by its truth table.
   The value is always between 0 and m where m is the number of inputs patterns."
  [circuit]
  (let [ks (keys circuit)
        outputs (map #(get circuit %1) ks)]
    (reduce +  (map #(* (p circuit %1) (log2 (/ 1 (p circuit %1)))) outputs))))

(defn mi
  "Mutual information between function T and R specified by their truth tables."
  [t r]
  )

(defn fitness
  "Fitness function for the Tofolli gate solution."
  [target candidate]
  )

(defn selection
  "Select which individuals advance based on a probability distribution defined by their fitness."
  [individuals]
  (let [max-fitness (max (map #(:fitness (meta %1)) individuals))]
    (filter #(< (/ (:fitness (meta %1)) max-fitness ) (rand)) individuals)))

(defn- breed
  "Takes two individuals and produces a single child based on those individuals.
   This occurs by randomly selecting an element from each position. This goes to
   the length of x, so inputs should be shuffled."
  [x y] 
  (map #((let [p (shuffle [x y])]  nth (first p) %1 (nth (second p) %1))) (range (count x))))

(defn crossover
  "Perform crossover on the current population in order to receive a new population."
  [population]
  (concat (map #(breed (first %1) (second %1)) (partition 2 (shuffle population)))
           (map #(breed (first %1) (second %1)) (partition 2 (shuffle population)))))

(defn- random-line
  "Selects a random line from the set of lines passed in. Basically a rand-nth that won't throw an exception."
  [lines]
  (cond
    (= 0 (count lines)) nil
    :else (rand-nth lines)))

(defn- mutate
  "Mutates a single Tofolli gate."
  [lines gate]
  (let [target-line (second gate)
        control-lines (set  (first gate)) 
        lines (set lines)
        unused-lines (difference lines control-lines #{target-line})
        action (rand-int 4)]
    (cond
      (= action 0) [(concat control-lines (random-line unused-lines)) target-line] ; add a control line
      (= action 1) [(rest (shuffle control-lines)) target-line] ; remove a control line
      (= action 2) [control-lines (rand-nth (difference lines control-lines))] ; change the target
      (= action 3) [(concat (rest (shuffle control-lines)) (random-line unused-lines )) target-line] ; remove and add a control line
  )))

(defn mutation
  "Mutates an individual at the specified mutation rate (mr).
   Intended to be used as the mutation function passed to the genetic algorithm."
  [lines mr individual]
  (if (< mr (rand))
    (let [mutation-idx (rand-int (count individual))]
      (concat (take mutation-idx individual) (mutate lines individual) (drop (+ 1 mutation-idx) individual)))
    individual))
