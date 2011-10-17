(ns tappan.matrix
  (:refer-clojure :exclude [rand spit slurp get seq >])
  (:import [java.util Random])
  (:import [org.ejml.simple SimpleMatrix]))

(defn constant
  "Creates a constant SimpleMatrix"
  ([n v] (constant n n v))
  ([n m ^double v]
     (doto (SimpleMatrix. n m)
       (.set v))))

(defn zeros
  "Creates a zero'd SimpleMatrix"
  [n & [m]]
  (constant n (or m n) 0))

(defn eye
  "Creates the identity matrix"
  [n]
  (SimpleMatrix/identity n))

(defn rand
  "Creates a randomized matrix"
  [n & [m min max]]
  (let [min (or min 0)
        max (or max 1)
        rand (Random.)]
    (SimpleMatrix/random n (or m n) min max rand)))

(defn from-sparse
  "Creates a matrix from a sparse representer of the form [[i j x]]."
  [n m data]
  (let [m0 (SimpleMatrix. n m)]
    (doseq [[i j v] data]
      (let [idx (.getIndex m0 i j)]
        (.set m0 idx v)))
    m0))

;;; Merging matrices
;;; 
(defn above
  "Attaches B to the bottom of A."
  [A B]
  (.combine A SimpleMatrix/END 0 B))

(defn then
  "Attaches B to the right of A."
  [A B]
  (.combine A 0 SimpleMatrix/END B))

;;; Obvious extraction functions.
;;; 
(defn size [m] [(.numRows m) (.numCols m)])
(defn nth-col [m n] (.extractVector m false n))
(defn nth-row [m n] (.extractVector m true n))
(defn get
  "Extracts a particular value from the matrix at pair index (i, j) or
  linear index i."
  ([m i] (.get m i))
  ([m i j]
     (let [idx (.getIndex m i j)]
       (get m idx))))

;;; TODO: How can I make this work?
;;; 
;; (extend-type org.ejml.simple.SimpleMatrix
;;   clojure.lang.AFn
;;   (invoke [this i j] (get this i j))
;;   (applyTo [this args] (clojure.lang.AFn/applyToHelper this args)))

(defmacro get*
  "Universal extractor: use :_ to indicate a wildcard on a particular dimension."
  [m i j]
  (cond (= i j :_) m
        (= j :_) `(nth-row ~m ~i)
        (= i :_) `(nth-col ~m ~j)
        true `(get ~m ~i ~j)))

(defmacro gets*
  "Universal lazy-seq extractor: use :_ to indicate a wildcard on a
  particular dimension."
  [m i j]
  (cond (= i j :_) `(let [mat# ~m
                          [n# d#] (size mat#)]
                      (iterator-seq
                       (.iterator mat# true 0 0 (dec n#) (dec d#))))
        (= j :_) `(let [mat# ~m
                        i# ~i
                        [n# d#] (size mat#)]
                    (iterator-seq
                     (.iterator mat# true i# 0 i# (dec d#))))
        (= i :_) `(let [mat# ~m
                        j# ~j
                        [n# d#] (size mat#)]
                    (iterator-seq
                     (.iterator mat# true 0 j# (dec n#) j#)))
        true `(let [mat# ~m
                    j# ~j
                    i# ~i]
                (iterator-seq
                 (.iterator mat# true i# j# i# j#)))))

(defn diag
  "Either extracts a diagonal or builds a diagonal matrix."
  [m]
  (cond
   ;; Extract the diagonal
   (isa? (class m) org.ejml.simple.SimpleMatrix)
   (map #(.get m % %) (range (apply min (size (zeros 3)))))
   ;; or, build a matrix from this diagonal
   (or (seq? m) (vector? m))
   (let [n (count m)]
     (from-sparse n n (map list (range n) (range n) m)))))

;;; Some basic operations
;;; 
(defn norm
  "Returns the norm of the SimpleMatrix x. If it's a vector this is
  the Euclidean norm. If a matrix, the Frobenius norm."
  [x]
  (.normF x))

(defn scale
  "Applies A <- s A for s in R"
  [s A]
  (.scale A s))

; TODO: Refactor this vector flipping business.
(defn sum
  "Computes the sum of two (or more) SimpleMatrix objects. If they're
   both vectors, they will be automatically transposed if necessary so
   as to match the shape of the first one"
  ([a] a)
  ([a b & more] (reduce sum (sum a b) more))
  ([a b]
     (if (and (.isVector a)
              (.isVector b))
       ;; Let's try to flip the vectors so that they're compatible
       (let [[na ma] (size a)
             [nb mb] (size b)]
         (if (and (== na mb)
                  (== ma nb)) ;; Oh, then let's flip it to match a
           (.plus a (.transpose b))
           (.plus a b)))
       ;; Else just fall through to EJML's plus
       (.plus a b))))

(defn diff
  "Computes the elementwise matrix difference of two (or more)
   SimpleMatrix objects. If both are vectors, then they will be
   automatically transposed as necessary so as to match the shape of
   the first one"
  ([a] a)
  ([a b]
     (if (and (.isVector a)
              (.isVector b))
       ;; Let's try to flip the vectors so that they're compatible
       (let [[na ma] (size a)
             [nb mb] (size b)]
         (if (and (== na mb)
                  (== ma nb)) ;; Oh, then let's flip it to match a
           (.minus a (.transpose b))
           (.minus a b)))
       ;; Else just fall through to EJML's minus
       (.minus a b))))

;;; Data matrix operations
;; For the most part by convention data will be matrices in "example
;; by feature" order. N vectors in D-dimensional space forms an NxD
;; matrix.

(defmacro mapdat
  "Creates lazy looping context binding var to each row of data."
  [[var data] & body]
  `(let [data# ~data
         [n# d#] (size data#)]
       (map (fn [idx#]
              (let [~var (get* data# idx# :_)]
                ~@body))
            (range n#))))

;;; file IO
(defn spit
  "Spits a matrix to a file."
  [f matrix]
  (.saveToFile matrix (str f)))

(defn slurp
  "Slurps up a saved matrix file."
  [f]
  (SimpleMatrix/load f))