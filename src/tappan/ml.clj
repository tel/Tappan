;;; A workspace for building machine learning algorithms in Tappan.
(ns tappan.ml
  (:use tappan.util)
  (:require [tappan.matrix :as m]))

(defn converge-by
  "Either converge according to take-while2'ing the pred or end after
  n steps."
  [n not-converged? coll]
  (last (take n (take-while2 not-converged? coll))))

;;; Kernel space k-means
;;; 
;;; Algorithm in:
;;; Dhillon IS, Guan YQ, Kulis B
;;; "Kernel k-means, Spectral Clustering and Normalized Cuts"
;;; KDD '04, August 22-25, 2004

(defn kmeans:ks
  "Computes k-means partitioning based only on a kernel
  representation (kfn i j) for i, j in {0, 1, ... n}. The kernel
  function is automatically memoized and symmetricized."
  [kfn n k & {:keys [wfn limit] :or {limit 200}}]
   ;; Set up the weight and kernel functions, including memoization
   ;; assuming that the kernel will be fairly tough to compute
  (let [w     (if wfn (memoize wfn) (constantly 1))
        kfn0  (memoize kfn)
        k     (fn [i j] (apply kfn0 (sort [i j])))]
    (converge-by
     limit (comp not =)
     ;; Can this possibly cycle? It depends on whether the equals test
     ;; can always find equal partitions. It's probably not terribly
     ;; fast at least, but may also cycle if the order of the
     ;; partition sets is unstable.
     (iterate
      (fn [part] ;; Let's improve this partition
        (let [block-scores ;; The third term of the mean weighting
              (map (fn [pi]
                     ;; Compute the third term of the mean-distance
                     ;; 1/n_j^2 (sum of in-block similarities)
                     (/ (+
                         ;; Off diagonal components of the block,
                         ;; taking advantage of symmetry
                         (* 2 (reduce + (for [b pi c pi :while (< b c)]
                                          (* (w b) (w c)
                                             (k b c)))))
                         ;; The diagonal components
                         (reduce + (for [b pi] (k b b))))
                        (Math/pow (reduce (fn [sum b] (+ sum (w b))) pi) 2)))
                   part)]
          (map reverse
               (vals
                ;; We'll build the new partition as a hash map which
                ;; is reduced across the indices
                (reduce (fn [new-part a]
                          (let [new-label
                                (indmin ;; minimize the distance from the mean
                                 (map (fn [pi block-score]
                                        (let [line-score ;; The second term of the mean wt
                                              (* 2 (/ (reduce (fn [sum b] (+ sum (w b))) pi))
                                                 (reduce (fn [score b]
                                                           (+ score (* (w b) (k a b)))) pi))]
                                          (- block-score line-score)))
                                      part block-scores))]
                            (update-in new-part [new-label]
                                       (flip cons) a)))
                        (hash-map) (range n))))))
      ;; And initialize it by taking the first and second halves of
      ;; the indices
      (partition-all (/ n 2) (range n))))))


;;; Vector space k-means
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