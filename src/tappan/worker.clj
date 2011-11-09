(ns tappan.worker
  (:use clojure.tools.cli)
  (:require [somnium.congomongo :as cm]
            [die.roboter :as roboter]))

(defn mongo-group []
  (group "--mongo"
         (optional ["-h" "--host" "Mongo host"
                    :default "127.0.0.1"])
         (optional ["-p" "--port" "Mongo server port"
                    :default "13241"]
                   #(Integer/parseInt %))
         (optional ["-db" "--database" "Database name"
                    :default "gold"]
                   keyword)))
(defn roboter-group []
  (group "--roboter"
         (optional ["-h" "--host" "AMQP host"])
         (optional ["-p" "--port" "AMQP port"
                    :default "11300"]
                   #(Integer/parseInt %))
         (optional ["-u" "--username" "AMQP user"
                    :default "guest"])
         (optional ["-x" "--password" "AMQP password"
                    :default "guest"])))

(defmacro with-invoked-mongo [inv & body]
  `(let [mongo# (:mongo ~inv)]
     (cm/with-mongo (cm/make-connection (:database mongo#) mongo#)
       ~@body)))

(defn do-the-roboter
  "Turn into a Roboter using the invocation configuration"
  [inv]
  (roboter/work
   (select-keys (:roboter inv)
                (for [[k v] (:roboter inv) :when v] k))))

(defn die-in [t]
  (future
    (Thread/sleep (* t 1000))
    (System/exit 0)))