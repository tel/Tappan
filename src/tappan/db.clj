(ns clj.db
  (:require [somnium.congomongo :as cm]))


;; (cm/with-mongo (cm/make-connection :gold {:port 13241})
;;   (let [path "/home/hltcoe/ajansen/discbench/swbd/words_lc.lst"]
;;     (let [utterances
;;           (map-indexed #(zipmap [:line :word :convside :sbid :speaker_id :start :end]
;;                                 (conj (seq (split %2 #" ")) %1))
;;                        (split-lines (slurp path)))]
;;       (doseq [u utterances]
;;         (cm/insert!
;;          :utterances
;;          {:line (:line u)
;;           :word (:word u)
;;           :convside (read-string (:convside u))
;;           :sbid (:sbid u)
;;           :speaker (read-string (:speaker_id u))
;;           :start (read-string (:start u))
;;           :end (read-string (:end u))})))))

;; (def spkrs
;;   (let [path "/home/hltcoe/ajansen/discbench/swbd/spkr_gender.tab"
;;        spkrs (apply
;;               merge (map
;;                      (comp (fn [[id gender]]
;;                              {(read-string id) (read-string gender)})
;;                            #(str/split % #" "))
;;                      (str/split-lines (slurp path))))]
;;    spkrs))

;; (doseq [u (cm/fetch :utterances)]
;;   (cm/update!
;;    :utterances u
;;    (merge u {:gender (get spkrs (:speaker u))})))

;; (spit "/home/hltcoe/jabrahamson/goldgraph.gdf"
;;  (with-out-str
;;    (println "nodedef> name,word VARCHAR(32),speaker INT,gender INT")
;;    (doseq [u (cm/fetch :utterances)]
;;      (let [{:keys [line word speaker gender]} u]
;;        (println (format "l%s,%s,%s,%s" line word speaker gender))))
;;    (println "edgedef> node1,node2,weight,ingender INT,inspeaker INT,inword INT")
;;    (doseq [e (cm/fetch :edges)]
;;      (let [{:keys [lines dtw ingender inspeaker inword]} e]
;;        (println
;;         (format "l%s,l%s,%s,%s,%s"
;;                 (lines 0)
;;                 (lines 1)
;;                 (/ dtw)
;;                 ingender inspeaker inword))))))

;; (defn boolint [bool]
;;   (if bool 1 0))

;; (let [seed "university"]
;;   (cm/with-mongo (cm/make-connection :gold {:port 13241})
;;     (let [edges0 (cm/fetch :edges :where {:words seed})
;;           words (set (flatten (map :words edges0)))
;;           edges (filter #(some (partial = seed)
;;                                (:words %))
;;                         (flatten (map #(cm/fetch :edges :where {:words %})
;;                                       words)))
;;           lines (set (flatten (map :lines edges)))
;;           nodes (map #(cm/fetch-one :utterances :where {:line %}) lines)]      
;;       (spit
;;        "/home/hltcoe/jabrahamson/goldgraph_subset2.gdf"
;;        (with-out-str
;;          (println "nodedef> name,word VARCHAR(32),speaker INT,gender INT")
;;          (doseq [n nodes]
;;            (let [{:keys [line word speaker gender]} n]
;;              (println (format "l%s,%s,%s,%s" line word speaker gender))))
;;          (println "edgedef> node1,node2,weight,ingender INT,inspeaker INT,inword INT")
;;          (doseq [e edges]
;;            (let [{:keys [lines dtw ingender inspeaker inword]} e]
;;              (println
;;               (format "l%s,l%s,%s,%s,%s,%s"
;;                       (lines 0)
;;                       (lines 1)
;;                       (/ dtw)
;;                       (boolint ingender)
;;                       (boolint inspeaker)
;;                       (boolint inword))))))))))

;;; Some matrix things

(require '[org.ujmp.core MatrixFactory Matrix])

(defn acoord
  [i j] (into-array Long/TYPE [i j]))

(defn zeros-m
  "Build a dense matrix of zeros."
  ([n] (zeros-m n n))
  ([n m] (MatrixFactory/dense (acoord n m))))

(defn count-m [m]
  (seq (.getSize m)))

(defn set-m
  "Sets the value of the matrix at a particular coordinate."
  ([m ^double val ^long i ^long j]
     (.setAsDouble m val (acoord i j))))

(defn update-m
  [m f ^long i ^long j]
  (set-m m (f (.getAsDouble m (acoord i j))) i j))

(defn diag-m
  [m]
  (let [n (apply min (seq (.getSize m)))]
    (map #(.getAsDouble m (acoord % %)) (range n))))

(defn col-of-m
  [m index]
  (let [[N M] (seq (.getSize m))]
    (vec (map #(.getAsDouble m (acoord % index)) (range N)))))

(defn row-of-m
  [m index]
  (let [[N M] (seq (.getSize m))]
    (vec (map #(.getAsDouble m (acoord index %)) (range M)))))

(defn graph-near
  "Returns the nodes and edges \"visible\" from a particular seed
  word. These include all utterances of the seed word, all utterances
  connected by at least one edge to utterances of the seed word and
  all edges between nodes specified thus far."
  [seed]
  (let [edges0 (cm/fetch :edges :where {:words seed})
        words (set (flatten (map :words edges0)))
        edges (mapcat #(cm/fetch :edges :where
                                 {:words %})
                      (for [w1 words w2 words]
                        [w1 w2]))
        nodes (mapcat #(cm/fetch :utterances :where {:line %})
                      (set (mapcat :lines edges)))]
    [nodes edges]))

;;; Make Laplacian
;;; 
(defn get-laplacian
  [nodes edges]
  (let [N (count nodes)
        node-map (zipmap (map :line nodes) (range (count nodes)))
        L (zeros-m N)]
    ;; Weird iterative pattern---
    (doseq [e edges]
      (let [[i j] (map node-map (e :lines))
            val (/ (e :dtw))
            val 1]
        (set-m L (- val) i j)
        (set-m L (- val) j i)
        (update-m L (partial + val) i i)
        (update-m L (partial + val) j j)))
    ;; Return it!
    L))

(defn laplacian-near [seed]
  (apply get-laplacian (graph-near seed)))