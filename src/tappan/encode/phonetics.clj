(ns tappan.encode.phonetics)

;;; Uses the Byblos dictionary to create a phonetic mapping
;;; 

(def path "data/")
(def name "Fisher_Eval03_2300hrs")
(def dict (doall (map second (re-seq #"([^\n]+)\n" (slurp (str path name ".dict.sil"))))))

(def phonemes
  ;; Creates a phoneme->hash mapping where hashes are just the
  ;; phonemes as listed in the .phonemes file mapped against [A-Za-z].
  (apply merge
         (map-indexed (fn [i pair]
                        (let [idx0 (+ i 65)
                              idx (if (> idx0 90)
                                    (+ idx0 7) idx0)]
                          {(second pair) (char idx)}))
                      (re-seq #"([^\n]+)\n" (slurp (str path name ".phonemes"))))))

(defn to-hash [pron]
  (apply str (map (comp phonemes second) (re-seq #"([^-]+)-?" pron))))

(def hdict
  ;; A very nice dictionary between words and their pronunciations!
  (apply merge
         (map (fn [line]
                (let [[_ word keys] (re-find #">([^\s]+)\s([^\s].*)" line)
                      keys (map second (re-seq #"\s?([^\s]+)" keys))]
                  {word (map to-hash keys)}))
              dict)))

(defn get-hashes
  "Uses the hdict to find some hashes"
  [word]
  (hdict (.toUpperCase word) nil))

;;; i.e.
;;; 
;; (doseq [{:keys [word _id]} (cm/fetch :words :only [:word])]
;;   (let [hashes (tmp/get-hashes word)]
;;     (when (not (empty? hashes))
;;       (cm/update! :words {:_id _id} {:$set {:phonetics hashes}}))))