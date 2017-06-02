(ns grpc.server
  (:require [grpc.transformer :refer [<-message]]))


(defmacro defrpc [name interfaces]
  `(do
     (defmacro ~(symbol (str "defn-" name)) [~'name ~'[request response & options] ~'& body#]
       (let [mname# ~(str name)]
         `(update-proxy ~(symbol mname#) {~(str ~'name) (fn [~'~'this ~~'request ~~'response ]
                                                          (let [~~'request  (<-message ~~'request)]
                                                            ~@body#
                                                            (.onCompleted ~~'response)))})))
     (def ~name
       (proxy [~@interfaces] []))))
