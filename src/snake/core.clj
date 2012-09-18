(ns snake.core
  (:require [clojure.set :refer [difference intersection union]]))

(def ^:dynamic *playing-field* [20 20])

(defn next-dest [[head trail & _]]   (map - head trail))
(defn wrap-around-world [dest]       (map mod dest *playing-field*))

(def default-snake-shape '([0 0] [1 0] [2 0]))

(defn dest [snake direction]
  (->> (case direction
         :up    [ 0 -1]
         :down  [ 0  1]
         :left  [-1  0]
         :right [ 1  0]
         (next-dest snake))
       (map + (first snake))))

(defn random-empty-pt [oldfood filled]
  (let [[max-x max-y] *playing-field*]
    (-> (set (for [x (range max-x)
                   y (range max-y)] [x y]))
        (difference (set oldfood) (set filled))
        (seq) (rand-nth))))

(defn apply-when [pred x f]
  (if (pred x) (f x) x))

(defn default-snake []
  (let [start-pt (map / *playing-field* (repeat 2))]
    (map (partial map + start-pt) default-snake-shape)))

(defn default-world []
  {:snake      (default-snake)
   :speed      1
   :food       (random-empty-pt nil (default-snake))
   :game-over? false})

(defn eat-self [app]
  (fn [{snake :snake :as world}]
    (app (cond (apply distinct? snake) world
               :else (assoc world :game-over? true)))))

(defn eat-food [app]
  (fn [{:keys [snake food speed] :as world}]
    (app (cond (empty? (intersection (set snake) #{food}))
               (assoc world :snake (butlast snake))
               :else
               (assoc world :food  (random-empty-pt #{food} snake)
                            :speed (inc speed))))))

(defn wrap [app]
  (fn [{snake :snake :as world}]
    (app (assoc world :snake (map wrap-around-world snake)))))

(defn move [app]
  (fn [{:keys [events snake] :as world}]
    (let [[event & other] events
          moved-snake     (cons (dest snake event) snake)]
      (app (assoc world :events other :snake moved-snake)))))

(defn restart-game [app]
  (fn [{:keys [events] :as world}]
    (cond (nil? world)           (default-world) ;; I realise this seems redundant,
          (= (first events) :f2) (default-world) ;; but they may differ in future.
          :else                  (app world))))

(defn game-running [app]
  (fn [world] (condp :game-over? world world
                    :else (app world))))

(def game (-> identity
           eat-self
           eat-food
           wrap
           move
           game-running
           restart-game))
