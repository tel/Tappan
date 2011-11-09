(ns tappan.workers.kmeans_purity
  (:use [tappan util ml worker dists]
        [clojure.tools cli]
        [clojure set]
        [cljfastdtw core])
  (:require [somnium.congomongo :as cm]
            [tappan.matrix :as m]
            [tappan.data.swbd :as swbd]
            [die.roboter :as roboter])
  (:gen-class))

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

(defn data-and-key
  "Get all the SWBD entries for a word pairing along with a key to
  differentiate their identities"
  [pair]
  (let [[u1 u2] (map get-data (:words pair))
        data (concat u1 u2)
        n1 (count u1)
        n2 (count u2)
        n  (count data)
        wordkey (apply merge
                       (concat
                        (map (fn [i] {i 0}) (range n1))
                        (map (fn [i] {i 1}) (range n1 n))))]
    [data wordkey]))

(defn espace-of
  ([affinity] (let [[n d] (m/size affinity)] (espace-of affinity n)))
  ([affinity n]
     (let [[n d] (m/size affinity)
           root-d (m/diag
                   (for [i (range n)]
                     (/ (Math/sqrt
                         (reduce
                          + (m/gets* affinity i :_))))))
           lap (m/prod root-d affinity root-d)
           {:keys [_ vecs]} (m/eig lap)]
       (apply m/above (map m/normalize (m/rows (apply m/then (take n vecs))))))))

(defn instance-distance-matrix [data]
  (let [n (count data)]
    (m/from-sparse
     n n
     (apply concat
            (for [i (range n) j (range i n)]
              (let [{:keys [swid start stop]} (nth data i)
                    o1 (swbd/read-post swid start stop)
                    {:keys [swid start stop]} (nth data j)
                    o2 (swbd/read-post swid start stop)
                    dist (m-dtw-dist o1 o2 10)]
                [[i j dist] [j i dist]]))))))

(defn compute-purity*
  "Find the purity between instances of a particular word pair using
  kernel-kmeans with a gram matrix computed as (dist-to-aff
  dtw-distances)."
  [pair dist-to-aff]
  (let [[data wordkey] (data-and-key pair)
        n (count data)
        aff (dist-to-aff (instance-distance-matrix data))
        kmeans-part (kmeans:ks (partial m/get aff) n 2)
        pty (purity (map (fn [cluster] (map wordkey cluster)) kmeans-part))]
    (println "Purity between" (:words pair) "is:" pty)))

(defn compute-purity-kmeans
  ([pair] (compute-purity-kmeans
           pair #(Math/exp (- (/ (Math/pow % 2)
                                 (* 2 20 20))))))
  ([pair simfn]
     (compute-purity*
      pair
      (partial m/map simfn))))

(defn compute-purity-espace
  ([pair] (compute-purity-espace
           pair #(Math/exp (- (/ (Math/pow % 2)
                                 (* 2 20 20))))))
  ([pair simfn]
     (compute-purity*
      pair
      (fn [D]
        (let [espace (espace-of
                      (m/map simfn D))]
          (m/prod espace (m/t espace)))))))

;; Levenshtein distances
;;;

(defn set-levenshtein [pairid]
  (let [pair (cm/fetch-one :worddist :where {:_id (cm/object-id pairid)})
        words (:words pair)
        w1 (cm/fetch-one :words :where {:word (get words 0)})
        w2 (cm/fetch-one :words :where {:word (get words 1)})
        ps1 (:phonetics w1)
        ps2 (:phonetics w2)
        editdists (for [p1 ps1 p2 ps2] (/ (levenshtein p1 p2) (+ (count p1) (count p2))))
        meanedit (if (empty? editdists)
                   nil
                   (/ (reduce + editdists) (count editdists)))]
    (println "D[" ps1 ps2 "] : " meanedit)
    (cm/update! :worddist pair {:$set {:levenshtein editdists
                                       :meanlevenshtein meanedit}})))

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

;;; The worker body
(defn -main [& args]
  (let [inv (cli args
                 (mongo-group)
                 (roboter-group))]
    (die-in (* 5 60 60)) ;; 60 minute run time
    (with-invoked-mongo inv
      (binding [roboter/*exception-handler*
                (fn [e msg]
                  (println "Oh, we got trouble!" (.getMessage e))
                  (System/exit 1))]
        (println "Robot: ONLINE!")
        (do-the-roboter inv)))))