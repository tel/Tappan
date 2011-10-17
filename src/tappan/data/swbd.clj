(ns tappan.data.swbd
  (:require [tappan.matrix :as m])
  (:import [java.io RandomAccessFile]
           [java.nio ByteBuffer ByteOrder]))

(def post-path*
  "/export/projects/ajansen/SWBDPOST/posteriors/")
(def post-N* 45)

(defn id->path
  "Converts a SWBD ID like \"2001A\" to its posterior file path like
  $PATH/20/sw02001_1.comb.binary."
  [swid]
  (str post-path*
       (subs swid 0 2) "/"
       "sw0" (subs swid 0 4) "_"
       (if (= "A" (subs swid 4 5))
         "1.comb.binary"
         "2.comb.binary")))

(defn read-post
  "Reads the posterior file referenced by switchboard id. Without
  specifying the beginning and end, it will read the entire file."
  [swid & [beg end]]
  (with-open [file (RandomAccessFile. (id->path swid) "r")]
    (let [len  (.length file)
          beg  (or beg 0)               ; frames
          end  (or end (/ len post-N*)) ; frames
          N (- end beg)
          bb (ByteBuffer/allocate 4)]
      (.seek file (* 4 beg post-N*))
      (m/from-sparse
       N post-N*
       (map (fn [[i j]]
              ;; This is a nonsense way to flip the endianness which is necessary.
              ;; So stupid.
              (.clear bb)
              (.order bb (ByteOrder/LITTLE_ENDIAN))
              (.putFloat bb (.readFloat file))
              (.order bb (ByteOrder/BIG_ENDIAN))
              [i j (.getFloat bb 0)])
            (for [i (range N) j (range post-N*)] [i j]))))))