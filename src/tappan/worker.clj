(ns tappan.worker
  (:use clojure.tools.cli)
  (:require [somnium.congomongo :as cm]))

(defmacro mongo-group []
  `(group "--mongo"
          (optional ["-h" "--host" "Mongo host"
                     :default "127.0.0.1"])
          (optional ["-p" "--port" "Mongo server port"
                     :default "13241"]
                    #(Integer/parseInt %))
          (optional ["-db" "--database" "Database name"
                     :default "gold"]
                    keyword)))

(defmacro with-invoked-mongo [inv & body]
  `(let [mongo# (:mongo ~inv)]
     (cm/with-mongo (cm/make-connection (:database mongo#) mongo#)
       ~@body)))