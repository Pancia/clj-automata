(ns clojure-automata.core
  (:import [javax.swing JPanel JFrame]
           [java.awt Color Graphics image.BufferedImage])
  (:use [seesaw.core :only [native! frame pack! show! 
                            border-panel canvas select 
                            button  flow-panel]]))

(set! *warn-on-reflection* true)
(native!)

(def dim-board [90 90])
(def dim-screen [600 600])
(def dim-scale (vec (map / dim-screen dim-board)))
(def %start-alive 30)

(defn new-board
  ([] (new-board dim-board))
  ([[x y]]
  (for [x (range x)]
    (for [y (range y)]
      (if (< (rand-int 100) %start-alive) :on :off)))))

(defn with-coords [board]
  (for [[row-idx row] (map-indexed vector board)]
    (for [[col-idx val] (map-indexed vector row)]
      [val row-idx col-idx])))

(def state->color {:on Color/WHITE,
                   :off Color/BLACK,
                   :dying Color/GRAY})

(defn render-cell [^Graphics graphics cell]
  (let [[state x y] cell
        [x-scale y-scale] dim-scale
        x (inc (* x x-scale))
        y (inc (* y y-scale))]
    (doto graphics
      (.setColor (state->color state))
      (.fillRect x y (dec x-scale) (dec y-scale)))))

(defn render-board [^Graphics graphics ^BufferedImage img board]
  (let [background-graphics (.getGraphics img)]
    (doto background-graphics
      (.setColor Color/BLACK)
      (.fillRect 0 0 (dim-screen 0) (dim-screen 1)))
    (doseq [row (with-coords board)
            cell row]
      (when-not (#{:off} (cell 0))
        (render-cell background-graphics cell)))
    (.drawImage graphics img 0 0 nil)))

(defn active-neighbors
  [[[nw n ne]
    [w  _ e ]
    [sw s se]]]
  (count
    (filter
      #{:on}
      (concat [nw n ne w e sw s se]))))

(defn game-of-life-rules
  [above [_ cell _ :as row] below]
  (if (= cell :dying)
    :off)
  (let [num-alive (+ (active-neighbors [above row below]) (if (= :on cell) 1 0))]
    (cond
      (= 3 num-alive) :on
      (= 4 num-alive) cell
      :else           :off)))

(defn brians-brain-rules
  [above [_ cell _ :as row] below]
  (cond
    (= :on cell)                               :dying
    (= :dying cell)                            :off
    (= 2 (active-neighbors [above row below])) :on
    :else                                      :off))

(def the-rules (atom game-of-life-rules))

(defn torus-window [coll]
  (partition 3 1 (concat [(last coll)] coll [(first coll)])))

(defn step
  [board]
  (doall
    (map (fn [window]
           (apply #(apply map @the-rules %&)
                  (doall (map torus-window window))))
         (torus-window board))))

(defn update-board!
  [board-ref]
  (swap! board-ref step))

(def is-running (atom true))
(defn activity-loop [^JPanel panel board]
  (while (and @board @is-running)
    (update-board! board)
    (.repaint panel)))

(def the-board (atom (new-board)))
(def the-panel (atom 0))

(defn resume! [] (swap! is-running (fn [_] true)))
(defn pause! [] (swap! is-running (fn [_] nil)))
(defn start! [] (do (swap! the-board (fn [_] (new-board))) (resume!)))
(defn stop! [] (do (pause!) (swap! the-board (fn [_] nil))))
(defn swap-rules! [] 
  (swap! the-rules 
    (fn [_] 
      (if (some #{:dying} (flatten @the-board)) 
        game-of-life-rules 
        brians-brain-rules))))

(defn seesaw-automata []
  (let [[screen-x screen-y] dim-screen
        img (BufferedImage. screen-x screen-y BufferedImage/TYPE_INT_ARGB)
        my-frame (frame :title "Clojure Automata" 
                     :content 
                        (border-panel :hgap 5 :vgap 5 :border 5
                          :center 
                            (canvas :id :canvas 
                              :paint (fn [c g] (render-board g img @the-board))
                              :size [screen-x :by screen-y])
                          :north  "NORTH"
                          :south  (flow-panel :vgap 10
                                    :items [(button :text "Resume!" 
                                              :listen [:action (fn [_] (resume!))])
                                            (button :text "Pause!" 
                                              :listen [:action (fn [_] (pause!))])
                                            (button :text "Start!" 
                                              :listen [:action (fn [_] (start!))])
                                            (button :text "Stop!"
                                              :listen [:action (fn [_] (stop!))])])
                          :east   "EAST"
                          :west   "WEST"))]
    (do (swap! the-board (fn [_] (new-board))))
    (-> my-frame pack! show!)
    (future (activity-loop (select my-frame [:#canvas]) the-board))
    (add-watch is-running :watch-change 
      (fn [_ _ _ newval] (if newval (future (activity-loop (select my-frame [:#canvas]) the-board)))))))

(defn -main []
  (seesaw-automata))


