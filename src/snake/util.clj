(ns snake.util)


(defmacro decision [name bind & body]
  `(defn ~name [app#] (fn [{:keys ~bind :as world#}]
                        (let [~'back (fn [& m#] (apply merge world# m#))
                              ~'cont (fn [& m#] (app# (apply merge world# m#)))]
                          ~@body))))
