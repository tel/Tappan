;;; Some obvious utility functions
(ns tappan.util)


;;; Convergence take
(defn take-while2
  "Like take-while but pred is applied to x(i-1) and x(i)."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [[x1 x2 & rest] s]
       (if x2
         (if (pred x1 x2)
           (cons x1 (take-while2 pred (cons x2 rest)))
           (list x1 x2))
         (list x1))))))

;;; Function utilities
;;;
(defn flip [f] (fn [a b] (f b a)))

;;; Optimizations
;;; 
(defn indopt
  "Find the index of the maximal value according to ord."
  [ord xs]
  (first
   (reduce (fn [[i max] [j val]]
             (if (ord val max)
               [j val]
               [i max]))
           (map list (range (count xs)) xs))))

(defn indmax
  "Find the index which maximizes the values in sequence xs."
  [xs] (indopt > xs))

(defn indmin
  "Find the index which maximizes the values in sequence xs."
  [xs] (indopt < xs))