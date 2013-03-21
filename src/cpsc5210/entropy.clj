(ns cpsc5210.entropy
  "This namespace will contain entropy and related functions."
  (:use (cpsc5210 util)))

(def stt
  [{:a 0 :b 0 :c 0 :d 1} ]
  )

(defn h
  ""
  [])

(defn binary-permutation
  "Takes a seq of input keywords and maps them to all possible binary permutations."
  [inputs]
  (loop [k inputs, accumulator [{}]]
    (cond 
      (empty? k) accumulator
      :else (recur (rest k)
                   (concat 
                     (map #(assoc %1 (first k) 0) accumulator)
                     (map #(assoc %1 (first k) 1) accumulator)))) ))

