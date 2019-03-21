(ns life-game.core)

(def Dead "-")
(def Live "*")

(defn deadOrDie
  [x]
  (if (= x 1)
    Live
    Dead))

(defn d2-nth
  [d2-lst x y]
  (when (and (not (or (neg? x) (neg? y)))
             (not (or (>= x (count d2-lst)) (>= y (count d2-lst)))))
    (nth (nth d2-lst y) x)))

;;; 指定座標周囲1マスの最大計9マスの状態を取得する。
;;; なお、取得できなかったマスはnilとして計上される。
(defn get-sel-around
  [d2-lst x y]
  (mapcat (fn [x_i] (map (fn [y_i] (d2-nth d2-lst x_i y_i))
                      (range (- y 1) (+ y 2))))
       (range (- x 1) (+ x 2))))

(defn d2-assoc
  [d2-lst x y v]
  (let [lst-to-vec (vec (map #(vec %) d2-lst))]
    (apply list (map #(apply list %) (assoc lst-to-vec y (assoc (nth lst-to-vec y) x v))))))

(defn fate-of-sel
  [d2-lst x y]
  (let [live-cnt (count (filter #(= % Live) (get-sel-around d2-lst x y)))]
    (cond
      (= live-cnt 2) (d2-assoc d2-lst x y Dead)
      (= live-cnt 3) (d2-assoc d2-lst x y Live)
      (= live-cnt 5) (d2-assoc d2-lst x y Dead)
      :else d2-lst)))

(defn -main
  []
  (let [n 10]
    (loop [g-map (map
                  (fn [x] (map
                           (fn [y] (deadOrDie (mod (rand-int (* x y 20045)) 2)))
                           (range n)))
                  (range n))]
      (println "--------------------------")
      (doall (map #(println %) g-map))
      (Thread/sleep 1000)
      (recur (reduce (fn [lst x] (fate-of-sel lst (nth x 0) (nth x 1)))
                     g-map
                     (mapcat (fn [x_i] (map (fn [y_i] (list x_i y_i))
                                         (range 0 n)))
                          (range 0 n))))
      )))
