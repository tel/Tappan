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

(defn spy [x] (prn x) x)

;;; TODO: Is there a way to generalize this to always initialize using
;;; regular k-means? Probably should just include initialization as a
;;; parameter.
;;; 
(defn kmeans:ks
  "Computes k-means partitioning based only on a kernel
  representation (kfn i j) for i, j in {0, 1, ... n}. The kernel
  function is automatically memoized and symmetricized."
  [kfn n nmeans & {:keys [wfn limit init] :or {limit 200}}]
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
      (fn [part]           ;; Let's improve this partition
        (let [block-scores ;; The third term of the mean weighting
              (map (fn [pi]
                     ;; Compute the third term of the mean-distance
                     ;; 1/n_j^2 (sum of in-block similarities)
                     (/ (+
                         ;; Off diagonal components of the block,
                         ;; taking advantage of symmetry
                         (* 2 (reduce + (for [b pi c pi :when (< b c)]
                                          (* (w b) (w c)
                                             (k b c)))))
                         ;; The diagonal components
                         (reduce + (for [b pi] (k b b))))
                        (Math/pow (reduce + (map w pi)) 2)))
                   part)]
          (vals
           ;; We'll build the new partition as a hash map which
           ;; is reduced across the indices
           (group-by (fn [a]
                       (indmin ;; minimize the distance from the mean
                        (map (fn [pi block-score]
                               (let [line-score ;; The second term of the mean wt
                                     (* 2 (/ (reduce + (map w pi)))
                                        (reduce + (map (fn [b]
                                                         (* (w b) (k a b)))
                                                       pi)))]
                                 (- block-score line-score)))
                             part block-scores))) (range n)))))
      ;; And initialize it by taking the first and second halves of
      ;; the indices
      (or init
          (partition-all (Math/ceil (/ n nmeans)) (range n)))))))


;;; Vector space k-means
(defn nearest-mean
  [datum means]
  (let [dists (map #(m/norm (m/diff datum %)) means)]
    (indmin dists)))

(defn initialize-by-random-points [data k]
  (let [[n d] (m/size data)
        indexes (repeatedly k #(rand-int n))
        means (map #(m/nth-row data %) indexes)]
    (vals (group-by #(nearest-mean (m/nth-row data %) means) (range n)))))

(defn kmeans
  "k-Means by Lloyd's algorithm"
  [data nmeans & {:keys [limit init] :or {limit 200}}]
  (let [[n d] (m/size data)]
    (converge-by
     ;; Can this cycle? Converges when the partition stops changing,
     ;; but with the exact same issues as above.
     limit (comp not =)
     ;; Iteratedly improve the partition
     (iterate (fn [part]
                (let [means (map #(m/scale
                                   (/ (count %))
                                   (apply m/sum
                                          (map (partial m/nth-row data) %)))
                                 part)]
                  (vals
                   (group-by #(nearest-mean (m/nth-row data %) means)
                             (range n)))))
              (or init ;; Use the initial clusters or find our own.
                  (partition-all (Math/ceil (/ n nmeans)) (range n)))))))