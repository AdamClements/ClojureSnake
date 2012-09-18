(ns snake.newcore (:require [snake.util :refer [decision]]
                            [clojure.set :as set]))

(def ^:dynamic *playing-field* [20 20])

(defn next-dest [[head trail & _]]   (map - head trail))

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
        (set/difference (set oldfood) (set filled))
        (seq) (rand-nth))))

(defn default-snake []
  (let [start-pt (map / *playing-field* (repeat 2))]
    (map (partial map + start-pt) default-snake-shape)))

(defn default-world []
  {:snake      (default-snake)
   :speed      1
   :food       (random-empty-pt nil (default-snake))
   :game-over? false})

(decision eat-self [snake]
  (if (apply distinct? snake) (cont)
      (back {:game-over? true})))

(decision eat-food [snake food speed]
  (cont (if (empty? (set/intersection (set snake) #{food}))
          {:snake (butlast snake)}
          {:food (random-empty-pt #{food} snake)
           :speed (inc speed)})))

(decision wrap [snake]
  (cont {:snake (map #(map mod dest *playing-field*) snake)}))

(decision move [events snake]
  (let [[event & other] events
        moved-snake (cons (dest snake event) snake)]
    (cont {:events other :snake moved-snake})))

(decision game-running [game-over]
          (if game-over (back {})
              (cont {})))

(decision restart-game [events]
  (if (= (first events) :f2) (default-world)
      (cont)))

(defn debug [app]
  (fn [req]
    (println req)
    (app req)))

(def game (-> identity
           ;eat-self
           ;eat-food
           ;wrap
           ;move
           game-running
           restart-game
           debug
           ))
