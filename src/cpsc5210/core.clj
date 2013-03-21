(ns cpsc5210.core
  (:use (cpsc5210 util)
        (clojure set)))

(defn ga
  "Main function for running the genetic algorithm.
   This will perform the fitness -> selection -> crossover -> mutation cycle
   until we have reached the maximum number of generations or our termination
   condition specified by terminate? is satisfied.
   
   Arguments:
   fitness : A function that can take an individual and return a fitness score for that individual.
   selection : A fn that takes a population (with fitness metadata) and performs selection returning a new population.
   crossover : A fn that takes a population and performs crossover, returning a new population.
   mutation : A fn which takes a population and mutation rate as a parameter and returns a mutated population.
   terminate? : A function that takes the population (with fitness metadata) and determines if the ga should terminate.

   intial-population : A collection representing the initial population.
   tmax : The maximum number of generations for which to run the algorithm.
   mr : The mutation rate. P(mutation)
   
   Returns: The population at the time of termination, with fitness metadata. "
  [fitness selection crossover mutation terminate? initial-population tmax mr]
  (loop [t 0, pt initial-population]
    (let [pt (map #(with-meta %1 {:fitness (fitness %1)}) pt)]
    (cond
      (or (> t tmax) (terminate? pt)) pt
      :else (recur 
              (+ t 1)
              (->>
                pt
               (map selection)
               (crossover)
               (map mutation mr)))))))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn- p 
  "Calculates the probability of a particular output given a circuit.
   Currently cheats and assumes we have a reversible circuit, thus the output
   occurs exactly once."
  [circuit output]
  (/ 1  (count (set (filter keyword? (flatten circuit))))))

(defn- p2
  ""
  []
  )

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
  "Mutates an individual at the specified mutation rate (mr)."
  [lines mr individual]
  (if (< mr (rand))
    (let [mutation-idx (rand-int (count individual))]
      (concat (take mutation-idx individual) (mutate lines individual) (drop (+ 1 mutation-idx) individual)))
    individual))
