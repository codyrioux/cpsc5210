(ns cpsc5210.rioux
  "This namespace contains all of the code implementing the Rioux paper.
   It publically defines functions: fitness, mutation, selection, crossover
   for use in the genetic algorithm defined in cpsc5211.core
   
   In addition to this the namespace utilizes an array of private information
   theory functions to implement experiment-specific fitness functions."
  (:use (cpsc5210 util)
        (clojure set)))

;; Helper Functions

(defn- log2 [n]
  "Returns the base 2 logarithm of n."
  (/ (Math/log n) (Math/log 2)))

(defn get-lines
  [circuit]
  (set (flatten (map keys (keys circuit)))))

(defn- random-line
  "Selects a random line from the set of lines passed in. Basically a rand-nth that won't throw an exception."
  [lines]
  (cond
    (= 0 (count lines)) [] 
    :else [ (rand-nth (seq lines))])) 

;; Entropy Functions

(defn- p 
  "Calculates the probability of a particular output vector of given a circuit.
   Currently cheats and assumes we have a reversible circuit, thus the output occurs exactly once.
   Returns: p(circuit=output)"
  [circuit output]
  (/ 1 (count (keys circuit))))

(defn p2
  "Calculates p(r=rval | t=tval).
   Determines the fraction of outputs in t where t=tval.
   Then determines the outputs of r given the inputs that provide t=tval,
   calculating the fraction of r=rval on those determined outputs.

   Inputs:
   ttable : The truth table representing the t circuit.
   rtable : The truth table representing the r circuit. (Can be the same table.)
   tkey : The keyword representing the bit in t for which we are interested.
   tval : The value for tkey in which we are interested.
   rkey : The keyword representing the bit in r for which we are interested.
   rval : The value for rkey in which we are interested.
   
   Returns: p(r=rval | t=tval) as a fraction."
  [ttable rtable tkey tval rkey rval]
  (let [t-outputs (map #(get ttable %1) (keys ttable))
        t-frac (/ (count (filter #(= tval (get %1 tkey)) t-outputs)) (count t-outputs))
        t-inputs-given-output (filter #(= (get (get ttable %1) tkey) tval)  (keys ttable))
        r-outputs (map #(get rtable %1) t-inputs-given-output) 
        r-frac (/ (count (filter #(= (get %1 rkey) rval) r-outputs)) (count r-outputs))]
    t-frac))

(defn h
  "Calculates the entropy of a given boolean function specified by its truth table.
   The value is always between 0 and m where m is the number of inputs patterns."
  [circuit]
  (let [ks (keys circuit)
        outputs (map #(get circuit %1) ks)]
    (* -1  (reduce +  (map #(* (p circuit %1) (log2 (p circuit %1))) outputs)))))

(defn h2
  "Calculates the joint entropy between input circuits t and r."
  [t r]
  (* -1 
     (reduce + 
      (map #(if (Double/isNaN %1) 0 %1)
      (for
       [tk (get-lines t)
        rk (get-lines r)
        tval [0 1]
        rval [0 1]]   
        (* (p2 t r tk tval rk rval) (log2 (p2 t r tk tval rk rval))))))))

(defn nmi
  "Calculates the normalized mutual information between two circuits r and t.
   Defined as (H(t) + H(r)) / H(t, r).
   
   Inputs:
   t : The truth table representing the t circuit.
   r : The truth table representing the r circuit.

   Returns: The normalized mutual information (NMI) of t and r."
  [t r]
  (/ (+ (h t) (h r)) (+ 1 (h2 t r))))

;; Genetic Algorithm Functions

(defn fitness
  "Fitness function for the Tofolli gate solution based on normalized mutual information (NMI)
   and quantum cost."
  [target candidate]
  (let [t-truth (tofolli-to-truth-table target)
        c-truth (tofolli-to-truth-table candidate)]
    (/ (/ 1 (nmi t-truth c-truth)) (+ 1 (circuit-quantum-cost candidate)))))

(defn fitness2
  "Fitness function for the Tofolli gate solution based on joint entropy and quantum cost."
  [target candidate]
  (let [t-truth (tofolli-to-truth-table target)
        c-truth (tofolli-to-truth-table candidate)]
    (/ (h2 t-truth c-truth) (+ 1 (circuit-quantum-cost candidate)))))

(defn fitness3
  "Fitness function for the Tofolli gate solution based on joint entropy."
  [target candidate]
  (let [t-truth (tofolli-to-truth-table target)
        c-truth (tofolli-to-truth-table candidate)]
    (h2 t-truth c-truth)))

(def fitness3 (memoize fitness3))

(defn selection
  "Select which individuals advance based on a probability distribution defined by their fitness."
  [individuals]
  (let [max-fitness (reduce max (map #(:fitness (meta %1)) individuals))]
    (filter #(< (rand) (/ (:fitness (meta %1)) max-fitness)) individuals)))

(defn- breed
  "Takes two circuits and performs crossover to produce a new solution the same length as max(length(x), length(y))."
  [x y]
  (let [shorter-length (min (count x) (count y))]
    (concat  (map #(nth (if (= (rand-int 2) 0) x y) %1) (range shorter-length)) (drop shorter-length x) (drop shorter-length y))))

(defn crossover
  "A crossover function that breeds each population once."
  [population]
  (map #(breed %1 (rand-nth population)) population))

(defn- mutate
  "Mutates a single Tofolli gate."
  [lines gate]
  (let [target-line (second gate)
        control-lines (set  (first gate)) 
        lines (set lines)
        unused-lines (difference lines control-lines #{target-line})
        action (rand-int 4)]
    (cond
      (= action 0) [(concat control-lines  (random-line unused-lines)) target-line] ; add a control line
      (= action 1) [(rest (shuffle control-lines)) target-line] ; remove a control line
      (= action 2) [(vec control-lines) (rand-nth (seq (difference lines control-lines)))] ; change the target
      (= action 3) [(concat (rest (shuffle control-lines))  (random-line unused-lines )) target-line] ; remove and add a control line
    )))

(defn mutation
  "Mutates an individual at the specified mutation rate (mr).
   Intended to be used as the mutation function passed to the genetic algorithm."
  [lines mr individual]
  (if (and (> (count individual) 0) (< (- 1 mr) (rand))) 
    (let [mutation-idx (rand-int (count individual))]
      (concat (take mutation-idx individual)  [ (mutate lines (nth individual mutation-idx))] (drop (+ 1 mutation-idx) individual)))
    individual))
