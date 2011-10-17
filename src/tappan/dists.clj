(ns tappan.dists
  (:use tappan.util)
  (:require [tappan.matrix :as m]))

(defn levenshtein
  "Computes the Levenshtein edit distance by flood filling the
   dissimilarity matrix."
  [s1 s2]
  (let [n1 (inc (count s1))
        n2 (inc (count s2))
        diss (m/from-sparse
              n1 n2
              (concat (map #(list % 0 %) (range n1))
                      (map #(list 0 % %) (range n2))))]
    (doseq [i (range 1 n1) j (range 1 n2)]
      (let [si (dec i)
            sj (dec j)]
        (m/set
         diss i j
         (if (= (get s1 si) (get s2 sj))
           (m/get diss (dec i) (dec j))
           (min (+ 1 (m/get diss (dec i) j))
                (+ 1 (m/get diss i (dec j)))
                (+ 1 (m/get diss (dec i) (dec j))))))))
    (m/get diss (dec n1) (dec n2))))