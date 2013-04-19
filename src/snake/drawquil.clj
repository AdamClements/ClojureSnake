(ns snake.drawquil
  (:require [quil.core :refer :all]
            [snake.core :refer :all]))

(defonce world (atom (default-world)))

(defn connected? [[a b]]
  (= 1 (abs (reduce + (map - a b)))))

(defn connect-lines [object]
  (apply concat (filter connected? (partition 2 1 object))))

(defn draw-snake [snake]
  (stroke 10 20 10)
  (begin-shape :lines)
  (doall (map #(apply vertex %) (connect-lines snake)))
  (end-shape))

(defn draw-food [[x y]]
  (stroke 0 198 34)
  (rect-mode :center)
  (rect x y 0 0))

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

(defn stack-events [& args]
  (let [new-key (key-as-keyword)
        {events :events} @world]
    (when (and (#{:f2 :up :down :left :right} new-key)
               (not= (first events) new-key))
      (swap! world assoc :events (cons new-key events)))))

(defn draw []
  (let [{:keys [game-over? snake food events speed]} (swap! world game)]
    (frame-rate (or speed 1))
    (background 0)
    (to-screen)
    (draw-background)
    (draw-snake snake)
    (draw-food food)
    (when game-over? (draw-gameover))))

(defn new-game []
 (defsketch snake-game
   :title "SNAKES"
   :key-pressed stack-events
   :setup #(frame-rate 1)
   :draw  draw))
