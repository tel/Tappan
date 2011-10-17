(ns tappan.workers.kmeans_purity
  (:use [tappan util ml worker]
        [clojure.tools cli]
        [cljfastdtw core])
  (:require [somnium.congomongo :as cm]
            [tappan.matrix :as m]
            [tappan.data.swbd :as swbd])
  (:gen-class))

;;; Use dist30p for useful knot coverage. This is 888 entries.

(defn get-data
  "Gets SWID START STOP for utterances corresponding to a particular word"
  [word]
  (map #(let [{:keys [sbid start end]} %]
          {:swid sbid :start start :stop end})
       (cm/fetch :utterances :where {:word word} :only [:sbid :start :end])))

(defn purity
  "Computes the cluster purity of a particular clustering of classes.
   Purity is the frequency of correct prediction, assuming cluster
   membership predicts for the majority class of that cluster."
  [clusters]
  (let [freqs  (map frequencies clusters)
        labels (map #(apply max-key % (keys %)) freqs)
        cts    (map (fn [lab set] (reduce + (map #(if (= lab %) 1 0) set)))
                    labels clusters)
        totals (map count clusters)]
    (float (/ (reduce + cts) (reduce + totals)))))

(defn compute-purity [n]
  (let [pairs (cm/fetch :worddist
                        :where {:kmeans_purity {:$exists false}
                                :dist20p {:$exists true}
                                :nedges {:$gte 3}}
                        :limit n)]
    (when pairs ;; nil out otherwise
      (doseq [pair pairs]
        (let [[u1 u2] (map get-data (:words pair))
              data (concat u1 u2)
              n1 (count u1)
              n2 (count u2)
              n (+ n1 n2)
              wordkey (apply merge
                             (concat
                              (map (fn [i] {i 0}) (range n1))
                              (map (fn [i] {i 1}) (range n1 n))))
              ;; Compute the kernel k-means using a exponential RBF
              ;; kernel on the distance
              variance (* 30 30)
              kmeans-part (kmeans:ks
                           #(let [{:keys [swid start stop]} (nth data %1)
                                  o1 (swbd/read-post swid start stop)
                                  {:keys [swid start stop]} (nth data %2)
                                  o2 (swbd/read-post swid start stop)
                                  dist (m-dtw-dist o1 o2 10)
                                  sim (Math/exp (- (/ (Math/pow dist 2)
                                                      (* 2 variance))))]
                              ;; (println [sim dist])
                              sim) n 2)
              pty (purity (map (fn [cluster] (map wordkey cluster)) kmeans-part))]
          (println "Purity between" (:words pair) "is:" pty)
          (cm/update! :worddist pair {:$set {:kmeans_purity pty}})))
      (count pairs))))

;;; I'll leave this overnight...
(def fut (future (compute-purity 275)))

(defn -main [& args]
  (let [inv (cli args
                 (mongo-group))]
    (with-invoked-mongo inv
      (loop []
        (if-let [pairs (compute-purity 10)]
          (recur))))))