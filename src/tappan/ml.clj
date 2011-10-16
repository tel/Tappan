;;; A workspace for building machine learning algorithms in Tappan.
(ns tappan.ml
  (:use tappan.util)
  (:require [tappan.matrix :as m]))

(defn nearest-mean
  [datum means]
  (let [dists (map #(m/norm (m/diff datum %)) means)]
    (indmin dists)))

(defn- improve-kmeans
  [data means]
  (let [labeled (reduce #(update-in %1 [(nearest-mean %2 means)] (flip cons) %2)
                        (hash-map) (m/mapdat [row data] row))]
    (map #(m/scale (/ (float (count %)))
                   (apply m/sum %))
         (vals labeled))))

(defn kmeans
  "k-Means by Lloyd's algorithm"
  [data k & [n]]
  (let [means (take k (m/mapdat [row data] row))]
    (nth (iterate (partial improve-kmeans data) means) (or n 20))))