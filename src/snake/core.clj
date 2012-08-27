(ns snake.core
  (:require [quil.core  :refer :all]
            [clojure.set :refer [difference
                                 union]]))

(def ^:dynamic *playing-field* [20 20])
(def default-snake '((10 10) (9 10) (8 10)))
(def food     (atom '(2 2)))
(def my-snake (atom default-snake
     :validator #(= (count %) (count (distinct %)))))

(defn next-dest [[head trail & _]]   (map - head trail))
(defn wrap-around-world [dest]       (map mod dest *playing-field*))

(defn dest [snake direction]
  (->> (case direction
         :up    [ 0 -1]
         :down  [ 0  1]
         :left  [-1  0]
         :right [ 1  0]
         (next-dest snake))
       (map + (first snake))
       (wrap-around-world)))

(defn random-empty-pt [oldfood filled]
  (let [[max-x max-y] *playing-field*]
    (-> (set (for [x (range max-x)
                   y (range max-y)] [x y]))
        (difference (set oldfood) (set filled))
        (seq) (rand-nth))))

(defn eat [desti snake]
  (swap! food random-empty-pt snake)
  (frame-rate (inc (current-frame-rate)))
  (cons desti snake))

(defn move [snake direction]
  (let [desti (dest snake direction)]
    (if (= desti @food)
      (eat desti snake)
      (cons desti (butlast snake)))))

(defn draw-food [[x y]]
  (stroke 0 198 34)
  (rect-mode :center)
  (rect x y 0 0))

(defn connected? [[a b]]
  (= 1 (abs (reduce + (map - a b)))))

(defn connect-lines [object]
  (apply concat (filter connected? (partition 2 1 object))))

(defn draw-snake [snake]
  (stroke 10 20 10)
  (begin-shape :lines)
  (doall (map #(apply vertex %) (connect-lines snake)))
  (end-shape))

(defn draw-background []
  (stroke 30 50 30)
  (fill 50 80 50)
  (rect-mode :corner)
  (apply rect -1/2 -1/2 *playing-field*))

(defn draw-gameover []
  (fill 200 30 30 200)
  (text-mode :model)
  (text-size 2)
  (text-align :center :center)
  (text "Game over\nF2?" 9.5 9.5))

(defn to-screen []
  (apply scale (map / (repeat 2 (min (width) (height))) *playing-field*))
  (translate 0.5 0.5))

(defn new-game! []
  (reset! my-snake default-snake)
  (frame-rate 3))

(defn draw []
  (background 0)
  (when (= :f2 (key-as-keyword)) (new-game!))

  (to-screen)
  (draw-background)
  (draw-snake @my-snake)
  (draw-food  @food)

  (try (swap! my-snake move (key-as-keyword))
  (catch IllegalStateException _
    (draw-gameover))))

(defsketch snake-game :title "SNAKES"
                      :setup new-game!
                      :draw  draw)
