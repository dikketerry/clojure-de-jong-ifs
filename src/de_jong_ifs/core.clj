(ns de-jong-ifs.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;(def a 2.0)
;(def b -2.0)
;(def g 2.0)
;(def d -2.0)

(defn transform [[x y] a b g d]
  [(- (q/sin (* a y)) (q/cos (* b x)))
   (- (q/sin (* g x)) (q/cos (* d y)))])

(defn setup []
  (q/frame-rate 60)
  (q/smooth)
  (q/color-mode :hsb 360 100 100)
  (q/stroke 120 20 88)
  (q/stroke-weight 2)
  (q/background 30)
  {:de-jong-ifs (let [x0 (rand)
                      y0 (rand)]
                  (iterate transform [x0 y0]))
   :frame-count 0
   :a 2
   :b -2
   :g 2
   :d -2
   :points []})

(defn update-state [state]
  (let [new-points (take 100 (:de-jong-ifs state))]
    (-> state
        (update :frame-count inc)
        (assoc :points (concat (:points state) new-points))  ;; Accumulate points
        (update :a #(mod (+ % 0.01) 100))
        (update :b #(mod (+ % 0.01) 100))
        (update :g #(mod (- % 0.01) 100))
        (update :d #(mod (- % 0.01) 100)))))

(defn draw [state]
  ; access state data
  (let [de-jong-ifs (:de-jong-ifs state)
        frame-count (:frame-count state)
        a (:a state)
        b (:b state)
        g (:g state)
        d (:d state)
        oscillation (* 100 (q/sin (/ frame-count 20)))]
    ;(q/background 240)
    (q/with-translation [(+ (/ (q/width) 2) oscillation) (/ (q/height) 2) ]
                        (let [points 500
                              scale 150]
                          (doseq [[x y] (take points (iterate #(transform % a
                           b g d) (first de-jong-ifs)))]
                            (q/point (* scale x) (* scale y)))))))

(q/defsketch de-jong-ifs
             :title "de-jong-ifs"
             :setup setup
             :update update-state
             :draw draw
             :size [800 800]
             :features [:keep-on-top]
             :middleware [m/fun-mode])

(defn -main [& args])