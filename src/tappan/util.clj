;;; Some obvious utility functions
(ns tappan.util)

(defn indopt
  "Find the index of the maximal value according to ord."
  [ord xs]
  (first
   (reduce (fn [[i max] [j val]]
             (if (ord val max)
               [j val]
               [i max]))
           (map list (range (count xs)) xs))))

(defn indmax
  "Find the index which maximizes the values in sequence xs."
  [xs] (indopt < xs))

(defn indmin
  "Find the index which maximizes the values in sequence xs."
  [xs] (indopt > xs))