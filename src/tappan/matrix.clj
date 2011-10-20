(ns tappan.matrix
  (:refer-clojure :exclude [rand spit slurp get set seq >])
  (:import [java.util Random])
  (:import [org.ejml.simple SimpleMatrix]))


;;; Axiomatic getting and setting
;;;
(defn get
  "Extracts a particular value from the matrix at pair index (i, j) or
  linear index i."
  ([m i] (.get m i))
  ([m i j]
     (let [idx (.getIndex m i j)]
       (get m idx))))

(defn set [m i j v]
  (let [idx (.getIndex m i j)]
    (.set m idx v)
    v))

;;; Constructors
;;; 
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
    (doseq [[i j v-or-f] data]
      (if (fn? v-or-f)
        ;; Do an update
        (set m0 i j (v-or-f (get m0 i j)))
        ;; Set the value
        (set m0 i j v-or-f)))
    m0))

;;; Merging matrices
;;; 
(defn above
  "Attaches B to the bottom of A."
  ([A] A)
  ([A B]
     (.combine A SimpleMatrix/END 0 B))
  ([A B & more]
     (reduce above A (cons B more))))

(defn then
  "Attaches B to the right of A."
  ([A] A)
  ([A B]
     (.combine A 0 SimpleMatrix/END B))
  ([A B & more]
     (reduce then A (cons B more))))

;;; Exploding matrices
;;;
(defn rows
  "Explodes a matrix into rows."
  [m] (let [[n d] (size m)] (map (partial nth-row m) (range n))))

(defn cols
  "Explodes a matrix into cols."
  [m] (let [[n d] (size m)] (map (partial nth-col m) (range d))))

;;; Obvious extraction functions.
;;; 
(defn size [m] [(.numRows m) (.numCols m)])
(defn nth-col [m n] (.extractVector m false n))
(defn nth-row [m n] (.extractVector m true n))

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
   (map #(.get m % %) (range (apply min (size m))))
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

(defn t
  "Transpose."
  [x] (.transpose x))

(defn permute-rows
  "Performs the row permutation."
  [X indices]
  (let [rows (vec (rows X))]
    (apply above (map (partial nth rows) indices))))

(defn permute-cols
  "Performs the row permutation."
  [X indices]
  (let [cols (vec (cols X))]
    (apply then (map (partial nth cols) indices))))

(defn permute
  "Performs the permutation P X Pt"
  [X indices] (permute-rows (permute-cols X indices) indices))

(defn dot
  "Returns the dot product, also generalized to matrices S_ij X_ij Y_ij"
  [X Y]
  (let [[nx dx] (size X)
        [ny dy] (size Y)]
    (if (and (= nx ny) (= dx dy))
      (reduce + (for [i (range nx) j (range dx)]
                  (* (get X i j) (get Y i j))))
      (throw (Exception. "Tensor sizes must be identical.")))))

(defn scale
  "Applies A <- s A for s in R"
  [s A]
  (.scale A s))

(defn normalize
  "Scale the matrix or vector so that it's norm is 1."
  [X]
  (let [s (norm X)
        [n d] (size X)]
    (from-sparse
     n d (for [i (range n) j (range d)]
           [i j (/ (get X i j) s)]))))

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

(defn prod
  "Computes the matrix product between a series of matrices."
  ([A] A)
  ([A B]
     (.mult A B))
  ([A B & more]
     (reduce prod (prod A B) more)))

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

(defmacro observations
  "Explodes the matrix into observations."
  [data]
  (rows data))

;;; Eigendecomposition
;;;
(defn- to-radial
  "Convert an imaginary number to the sequence [magnitude
  angle-in-radians].."  [z]
  [(.getMagnitude z)
   (Math/atan2 (.getImaginary z)
               (.getReal z))])

(defn eig [m]
  (let [[n d] (size m)]
    (if (not= n d)
      (throw (Exception.
              "Cannot do eigendecomposition on a non-square matrix.")))
    (let [evd (.eig m)
          nev (.getNumberOfEigenvalues evd)
          vv  (sort (fn [& things]
                      (apply compare
                             (map (comp #(.getMagnitude %) first) things)))
                    (map #(list (.getEigenvalue evd %)
                                (.getEigenVector evd %))
                         (range nev)))
          vals (map (comp to-radial first) vv)
          vecs (map second vv)]
      {:vals vals :vecs vecs :quality (.quality evd)})))

;;; file IO
;;; 
(defn spit
  "Spits a matrix to a file."
  [f matrix]
  (.saveToFile matrix (str f)))

(defn slurp
  "Slurps up a saved matrix file."
  [f]
  (SimpleMatrix/load f))