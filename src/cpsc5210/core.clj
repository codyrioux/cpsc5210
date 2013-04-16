(ns cpsc5210.core
  (:use (cpsc5210 util)
        (clojure set))
  (:gen-class))

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
  (loop [t 1, pt initial-population]
    (let [pt (map #(with-meta %1 {:fitness (fitness %1)}) pt)]
    (println (str "Generation " t " [" (count pt) "]"))
    (cond
      (or (> t tmax) (terminate? pt)) [t pt] 
      :else (recur 
              (+ t 1)
              (->>
                pt
               (selection)
               (crossover)
               (map #( mutation mr %1))))))))
