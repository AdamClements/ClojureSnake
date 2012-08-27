(ns snake.drawquil
  (:require [quil.core :refer :all]
            [snake.core :refer :all]))

(def ^:dynamic *playing-field* [50 50])

(defn draw-block 
  [x y]
  (let [[w h] (map / [(width) (height)] *playing-field*)]
    (rect (* x w) (* y h) w h)))

(defn draw-object
  [object]
  (doall (map draw-block object)))

(defsketch snake-game
           :title "SNAKES"
           :draw  draw
           :setup (fn [] (frame-rate 2)))