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
  ([affinity n-vecs]
     (let [[n d] (m/size affinity)
           root-d (m/diag
                   (for [i (range n)]
                     (/ (Math/sqrt
                         (reduce
                          + (m/gets* affinity i :_))))))
           lap (m/prod root-d affinity root-d)
           {:keys [_ vecs]} (m/eig lap)]
       (apply m/above (map m/normalize (m/rows (apply m/then (take n-vecs vecs))))))))

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
    pty))

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

(defn- order [vec]
  (let [keyed (map-indexed list vec)]
    (map first (sort-by second keyed))))

(defn- is-sparse-vec
  [[x & _]] (not (sequential? x)))

(defn- flatten-to-sparse* [vs]
  (loop [vs vs build nil]
    (let [[v & vs] vs]
     (if (nil? v)
       (vec build)
       (if (is-sparse-vec v)
         (recur vs (cons v build))
         (recur vs (concat (flatten-to-sparse* v) build)))))))

(defn- flatten-to-sparse [vs]
  (reverse (flatten-to-sparse* vs)))

(defn dist-to-knn
  "Generate a kNN linkage affinity from a distance matrix. Not
  particularly speedy."
  [k D]
  (let [n (first (m/size D))]
    (m/from-sparse n n
     (flatten-to-sparse
            (for [i (range n)]
              (let [indices (take (inc k) (order (m/gets* D i :_)))]
                (map (fn [j] [[i j 1] [j i 1]]) indices)))))))

(defn set-dist-attr [id fn key]
  (let [pair (cm/fetch-one :worddist
                           :where {:_id (cm/object-id id)}
                           :only [:words])]
    (cm/update! :worddist pair {:$set {key (fn pair)}})))

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

(defn write-stats [f & keys]
  (let [k2s  #(subs (str %) 1)
        blankify #(or % " ")
        keys0 keys
        basic-keys [:words :nedges :npairs :meanlevenshtein]
        keys (concat basic-keys keys0)
        pairs (cm/fetch :worddist
                        :where (apply merge
                                      (map (fn [k] {k {:$exists true}})
                                           ;; We accept nils, but not
                                           ;; in the first key
                                           ;; (concat (take 1 keys0) basic-keys)
                                           keys))
                        :only keys)
        _ (println (count pairs))
        fmt (apply str "%s, %s, %s, %s, "
                   (interpose ", " (repeat (count keys0) "%s")))
        comps (map #(map (comp blankify
                               (partial get %))
                         keys)
                   pairs)]
    (spit
     f (with-out-str
         (println (apply format fmt (map k2s keys)))
         (doseq [pair pairs]
           (let [[[w1 w2] & rest] (map (partial get pair) keys)]
             (println (apply format fmt (str w1 " " w2) rest))))))))

(defn output-dists []
  (write-stats "data/dists.csv"
               :espace_purity
               :espace_purity_knn2
               :espace_purity_knn8
               :espace_purity_sigma10
               :espace_purity_sigma5
               :kmeans_purity
               :kmeans_purity_knn2
               :kmeans_purity_knn8
               :kmeans_purity_sigma10
               :kmeans_purity_sigma5))

;;; The worker body
(defn -main [& args]
  (let [inv (cli args
                 (optional ["-t" "--ttl" "Time to live in minutes."
                            :default (str (* 5 60))]
                           #(Float/parseFloat %))
                 (mongo-group)
                 (roboter-group))]
    (die-in (* (:ttl inv) 60)) ;; 60 minute run time
    (with-invoked-mongo inv
      (binding [roboter/*exception-handler*
                (fn [e msg]
                  (println "Oh, we got trouble!" (.getMessage e))
                  (System/exit 1))]
        (println "Robot: ONLINE!")
        (do-the-roboter inv)))))

(def task-map
  {:espace_purity (fn [pair] `(compute-purity-espace ~pair))
   :kmeans_purity (fn [pair] `(compute-purity-kmeans ~pair))
   ;; Using only two eigenvectors
   :espace_purity2 (fn [pair]
                     `(compute-purity* ~pair
                        (fn [D#]
                          (let [espace# (espace-of
                                         (m/map simfn D) 2)]
                            (m/prod espace# (m/t espace#))))))
   :espace_purity_knn2 (fn [pair]
                         `(compute-purity* ~pair
                            (fn [D#]
                              (let [knn# (dist-to-knn 2 D#)
                                    espace# (espace-of
                                             (m/map simfn D))]
                                (m/prod espace# (m/t espace#))))))
   :espace_purity_knn8 (fn [pair]
                         `(compute-purity* ~pair
                            (fn [D#]
                              (let [knn# (dist-to-knn 8 D#)
                                    espace# (espace-of
                                             (m/map simfn D))]
                                (m/prod espace# (m/t espace#))))))
   :espace_purity_sigma10 (fn [pair] `(compute-purity-espace ~pair
                                        #(Math/exp (- (/ (Math/pow % 2)
                                                         (* 2 10 10))))))
   :espace_purity_sigma5 (fn [pair] `(compute-purity-espace ~pair
                                       #(Math/exp (- (/ (Math/pow % 2)
                                                        (* 2 5 5))))))
   :kmeans_purity_knn2 (fn [pair]
                         `(compute-purity* ~pair
                            (partial dist-to-knn 2)))
   :kmeans_purity_knn8 (fn [pair]
                         `(compute-purity* ~pair
                            (partial dist-to-knn 8)))
   :kmeans_purity_sigma10 (fn [pair] `(compute-purity-kmeans ~pair
                                        #(Math/exp (- (/ (Math/pow % 2)
                                                         (* 2 10 10))))))
   :kmeans_purity_sigma5 (fn [pair] `(compute-purity-kmeans ~pair
                                       #(Math/exp (- (/ (Math/pow % 2)
                                                        (* 2 5 5))))))})