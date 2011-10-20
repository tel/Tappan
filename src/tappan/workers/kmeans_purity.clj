(ns tappan.workers.kmeans_purity
  (:use [tappan util ml worker dists]
        [clojure.tools cli]
        [clojure set]
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

(defn eigenspace-of [words n]
  (let [ee (apply concat
                  (for [w1 words w2 words]
                    (cm/fetch :edges :where {:words [w1 w2]})))
        lines (set (mapcat :lines ee))
        nlines (count lines)
        line->id (apply merge
                        (map-indexed (fn [i z] {z i})
                                     lines))
        id->line (map-invert line->id)]
    (when (not (empty? lines))
      (let [W (m/from-sparse nlines nlines
                             (mapcat (fn [e]
                                       (let [[l1 l2] (map line->id (:lines e))
                                             dist (:dtw e)
                                             w (Math/exp (- (/ (* dist dist) (* 2 0.1 0.1))))]
                                         [[l1 l2 w]
                                          [l2 l1 w]
                                          [l1 l1 (/ w 2)]
                                          [l2 l2 (/ w 2)]]))
                                     ee))
            line->label (apply merge
                               (map (fn [line]
                                      {line
                                       (:word (cm/fetch-one :utterances
                                                            :where {:line line}))})
                                    lines))
            labels (map (fn [i] (-> i id->line line->label)) (range nlines))
            Dhalf (m/diag (map #(/ (Math/sqrt (reduce + (m/gets* W % :_)))) (range nlines)))
            lap (m/prod Dhalf W Dhalf)
            {:keys [vals vecs]} (m/eig lap)
            espace0 (apply m/then (take n vecs))
            espace (apply m/above (map m/normalize (m/rows espace0)))]
        [labels espace lap]))))

(defn compute-purity [n]
  (let [pairs (cm/fetch :worddist
                        :where {:kmeans_purity 0
                                :dist20p {:$exists true}
                                :nedges {:$gte 3}
                                :identical false}
                        :limit n)]
    (when pairs ;; nil out otherwise
      (doseq [pair pairs]
        (println (:words pair))
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
              variance (* 20 20)
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

(defn compute-purity-espace [n]
  (let [pairs (cm/fetch :worddist
                        :where {:espace_purity 0
                                :dist20p {:$exists true}
                                :nedges {:$gte 3}
                                :identical false}
                        :limit n)]
    (when pairs ;; nil out otherwise
      (doseq [pair pairs]
        (println (:words pair))
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
              variance (* 20 20)
              W (m/from-sparse
                 n n
                 (apply concat
                        (for [i (range n) j (range i n)]
                          (let [{:keys [swid start stop]} (nth data i)
                                o1 (swbd/read-post swid start stop)
                                {:keys [swid start stop]} (nth data j)
                                o2 (swbd/read-post swid start stop)
                                dist (m-dtw-dist o1 o2 10)
                                sim (Math/exp (- (/ (Math/pow dist 2)
                                                    (* 2 variance))))]
                            [[i j sim] [j i sim]]))))
              Dhalf (m/diag (map #(/ (Math/sqrt (reduce + (m/gets* W % :_)))) (range n)))
              lap (m/prod Dhalf W Dhalf)
              {:keys [vals vecs]} (m/eig lap)
              espace0 (apply m/then (take n vecs))
              espace (apply m/above (map m/normalize (m/rows espace0)))              
              kmeans-part (kmeans:ks #(m/dot (m/nth-row espace %1) (m/nth-row espace %2)) n 2)
              pty (purity (map (fn [cluster] (map wordkey cluster)) kmeans-part))]
          (println "Purity between" (:words pair) "is:" pty)
          (cm/update! :worddist pair {:$set {:espace_purity pty}})))
      (count pairs))))

;; (defn compute-purity-espace [n]
;;   (let [pairs (cm/fetch :worddist
;;                         :where {:espace_purity 0
;;                                 :dist20p {:$exists true}
;;                                 :nedges {:$gte 3}
;;                                 :identical false}
;;                         :limit n)]
;;     (when pairs ;; nil out otherwise
;;       (doseq [pair pairs]
;;         (let [[labels espace] (eigenspace-of (:words pair) 10000)
;;               z labels
;;               kmeans-part (kmeans:ks #(m/dot (m/nth-row espace %1) (m/nth-row espace %2))
;;                                      (count labels) 2)
;;               pty (purity (map (fn [cluster] (map (partial nth labels) cluster)) kmeans-part))]
;;           (println "Purity between" (:words pair) "is:" pty)
;;           (cm/update! :worddist pair {:$set {:espace_purity pty}})))
;;       (count pairs))))


;;; I'll leave this overnight...
;; (def fut (future (compute-purity 130)))

(defn -main [& args]
  (let [inv (cli args (mongo-group))]
    (with-invoked-mongo inv
      (loop []
        (if-let [pairs (compute-purity 10)]
          (recur))))))

;; Levenshtein distances
;;; 
;; (let [pairs (cm/fetch :worddist
;;                       :where {:kmeans_purity {:$exists true}}
;;                       :only [:words])]
;;   (doseq [pair pairs]
;;     (let [words (:words pair)
;;           w1 (cm/fetch-one :words :where {:word (get words 0)})
;;           w2 (cm/fetch-one :words :where {:word (get words 1)})
;;           ps1 (:phonetics w1)
;;           ps2 (:phonetics w2)
;;           editdists (for [p1 ps1 p2 ps2] (/ (levenshtein p1 p2) (+ (count p1) (count p2))))
;;           meanedit (if (empty? editdists)
;;                      nil
;;                      (/ (reduce + editdists) (count editdists)))]
;;       (println "D[" ps1 ps2 "] : " meanedit)
;;       (cm/update! :worddist pair {:$set {:levenshtein editdists
;;                                          :meanlevenshtein meanedit}}))))

(defn output-dists []
  (spit "data/dists.csv"
        (apply str
               "words, nedges, mindist, dist20p, dist30p, maxdist, meanlevenshtein, espace_purity, espace_purity2, kmeans_purity, meandist\n"
               (map (fn [{:keys [words nedges mindist dist20p dist30p maxdist
                                 meanlevenshtein espace_purity espace_purity2 kmeans_purity meandist]}]
                      (format "%s %s, %d, %s, %s, %s, %s, %s, %s, %s, %s, %s\n"
                              (first words) (second words)
                              nedges (or mindist " ") (or dist20p " ") (or dist30p " ") (or maxdist " ")
                              (or meanlevenshtein " ") (or espace_purity " ") (or espace_purity2 " ") (or kmeans_purity " ") (or meandist " ")))
                    (cm/fetch :worddist
                              :where {:espace_purity {:$gt 0}
                                      :kmeans_purity {:$gt 0}
                                      :identical false})))))