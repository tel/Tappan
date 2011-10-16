(ns clj.q
  (:require [beanstalk.core :as bs]))

;;; TODO: Make this all work
;;;
;;; Generally the beanstalk.core library above is disfunctional for
;;; high throughput queuing. It is very easy to cause it to desync
;;; with the server! To rememdy this it'd probably be best to make all
;;; server interactions asynchronous via either futures or agents.
;;;
;;; But that's for some other time.

(defmacro with-beanstalk
  "Binds a local beanstalk ensuring the pipe is closed
  afterward. Watch for lazy computations!"
  [[var & {:keys [host port]
           :or {host "localhost" port 11300}}]
   body]
  `(let [host# ~host
         port# ~port
         ~var (bs/new-beanstalk host# port#)]
     (try 
       ~@body
       (finally
        (bs/close ~var)))))

(defn kwstr [kw]
  (subs (str kw) 1))

(defmacro in-tube [[beanstalk tube] & body]
  `(let [new-tube# ~tube
         old-tube# (:tube (bs/list-tube-used ~beanstalk) "default")]
     (if new-tube#
       (try
         (bs/use ~beanstalk (kwstr new-tube#))
         ~@body
         (finally
          (bs/use ~beanstalk old-tube#)))
       (do
         ~@body))))

(defn do-enqueue
  "Greedily enqueues all the jobs."
  [beanstalk jobs & {:keys [tube priority delay ttr]
                     :or [tube nil priority 2000 delay 0 ttr 65535]}]
  (in-tube [bs tube]
    (doseq [job jobs]
      (let [payload (pr-str job)
            N (count payload)]
          (bs/put beanstalk priority delay ttr N payload)))))

;;; This is a good idea
;;; 
;; (defmacro transact
;;   [[obj kestrel queue & {:keys [timeout] :or {:timeout 2000}}]
;;    body & [else]]
;;   `(let [timeout# ~timeout
;;          kestrel# ~kestrel
;;          queue#   ~queue]
;;      (if-let [~obj (read-string
;;                     (.get kestrel# (str queue# "/t=" timeout# "/open")))]
;;        ~obj
;;        ;; (try
;;        ;;   ~body
;;        ;;   ;; (catch java.lang.Throwable o#
;;        ;;   ;;   (.get kestrel# (str queue# "/abort"))
;;        ;;   ;;   (throw))
;;        ;;   (finally
;;        ;;    (.get kestrel# (str queue# "/close"))))
;;        ~else)))

;;; As is this!
;;; 
;; (defmacro do-jobs
;;   "Pulls jobs off a Kestrel queue, acking on the resolution of each
;;   loop"
;;   [[var kestrel queue & {:keys [timeout limit]
;;                          :or {:timeout 2000
;;                               :limit false}}]
;;    & body]
;;   `(loop [~var (.get ~kestrel (str ~queue "/t=" ~timeout "/open"))
;;           count# ~limit]
;;      (if (and ~var (or (not count#) (> count# 0)))
;;        (do
;;          ~@body
;;          (recur (.get ~kestrel (str ~queue "/t=" ~timeout "/close/open"))
;;                 (and count# (dec count#)))))))