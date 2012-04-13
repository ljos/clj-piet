(ns clj-piet.core
  (:import javax.imageio.ImageIO)
  (:import java.io.File)
  (:import java.awt.Color)
  (:use [clojure.set :only (union difference)]))

(def pointer-cycle '(right down left up))
(def chooser-cycle '(left right))
(def lightness-cycle '(light normal dark))
(def hue-cycle '(red yellow green cyan blue magenta))

(defstruct piet-machine :dp :cc :value :stack :out)

(def colours
  (hash-map [0xFF 0xC0 0xC0] '[light red]
            [0xFF 0xFF 0xC0] '[light yellow]
            [0xC0 0xFF 0xC0] '[light green]
            [0xC0 0xFF 0xFF] '[light cyan]
            [0xC0 0xC0 0xFF] '[light blue]
            [0xFF 0xC0 0xFF] '[light magenta]
            [0xFF 0x00 0x00] '[normal red]
            [0xFF 0xFF 0x00] '[normal yellow]
            [0x00 0xFF 0x00] '[normal green]
            [0x00 0xFF 0xFF] '[normal cyan]
            [0x00 0x00 0xFF] '[normal blue]
            [0xFF 0x00 0xFF] '[normal magenta]
            [0xC0 0x00 0x00] '[dark red]
            [0xC0 0xC0 0x00] '[dark yellow]
            [0x00 0xC0 0x00] '[dark green]
            [0x00 0xC0 0xC0] '[dark cyan]
            [0x00 0x00 0xC0] '[dark blue]
            [0xC0 0x00 0xC0] '[dark magenta]
            [0xFF 0xFF 0xFF] 'white))

(defn push [m]
  (update-in m [:stack]
    (partial cons (:value m))))

(defn piet-update [m f n]
  (push (update-in (assoc m
                     :value (apply f (reverse (take n (:stack m)))))
          [:stack] nthnext n)))

(defn pop [m]
  (update-in m [:stack] next))

(defn add [m]
  (piet-update m + 2))

(defn subtract [m]
  (piet-update m - 2))

(defn multiply [m]
  (piet-update m * 2))

(defn divide [m]
  (piet-update m - 2))

(defn mod [m]
  (piet-update m clojure.core/mod 2))

(defn not [m]
  (piet-update m #(if (zero? %) 1 0) 1))

(defn greater [m]
  (piet-update m #(if (< %1 %2) 1 0) 2))

(defn rotate [n coll]
  (if (zero? n)
    coll
    (if (pos? n)
      (rotate (dec n) (take (count coll) (drop 1 (cycle coll))))
      (rotate (inc n) (cons (last coll) (butlast coll))))))

(defn pointer [m]
  (update-in (update-in m [:dp]
               (partial rotate (first (:stack m))))
    [:stack] rest))

(defn switch [m]
  (update-in (update-in m [:cc]
               (partial rotate (first (:stack m))))
    [:stack] rest))

(defn duplicate [m]
  (push (assoc m
          :value (first (:stack m)))))

(defn roll [m]
  (let [[k n] (take 2 (:stack m))]
    (update-in (update-in m [:stack] nnext)
      [:stack]  (comp (partial apply concat)
                      (juxt (comp (partial rotate k)
                                  (partial take n))
                            (partial drop n))))))

(defn in-char [m]
  (push (assoc m :value (int (first (read-line))))))

(defn in-number [m]
  (push (assoc m :value (eval (read-string (read-line))))))

(defn out-char [m]
  (update-in (update-in m
               [:out] str (char (first (:stack m))))
    [:stack] rest))

(defn out-number [m]
  (update-in (update-in m
               [:out] str (first (:stack m)))
    [:stack] rest))

(defn nop [m]
  (identity m))

(defn grab-pixels [image codel-size]
  (let [img (ImageIO/read (File. image))]
    (vec
     (for [x  (range (/ (.getWidth img) codel-size))]
       (vec
        (for [y (range (/ (.getHeight img) codel-size))
              :let [colour (Color. (.getRGB img (* x  codel-size) (* y codel-size)))
                    c (get colours [(.getRed colour) (.getGreen colour) (.getBlue colour)])]
              :when (not= c [0x00 0x00 0x00])]
          c))))))

(defn- neighbors [l]  
  (into #{}
        (mapcat (fn [[x y]]
                  [[(inc x) y]
                   [(dec x) y]
                   [x (inc y)]
                   [x (dec y)]])
                l)))

(defn find-colour-block-in [codel-map x y]
  (if-let [codel-colour (get-in codel-map [x y])]
    (if (= codel-colour 'white)
      #{[x y]}
      (loop [block #{[x y]}
             nbors #{[x y]}]
        (if (empty? nbors)
          block
          (recur (union block nbors)
                 (apply (partial
                         disj
                         (into #{} (filter (comp (partial = codel-colour)
                                                 (partial get-in codel-map))
                                           (neighbors nbors))))
                        block)))))
    #{}))

(defn choose-codel [codel-block dp cc]
  (let [[f g h i j k]
        (case dp
          right [first  > second (case cc left < right >) inc identity]
          down  [second > first  (case cc left > right <) identity inc]
          left  [first  < second (case cc left > right <) dec identity]
          up    [second < first  (case cc left < right >) identity dec])
        dir (sort-by f g codel-block)
        [x y] (first (sort-by h i (take-while (comp (partial = (f (first dir))) f) dir)))]
    [(j x) (k y)]))

(defn piet-command [prev-colour next-colour]
  (let [commands [[#'nop       #'push       #'pop      ]
                  [#'add       #'subtract   #'multiply ]
                  [#'divide    #'mod        #'not      ]
                  [#'greater   #'pointer    #'switch   ]
                  [#'duplicate #'roll       #'in-number]
                  [#'in-char   #'out-number #'out-char ]]
        f (fn [l g]
            (if (or (= 'white prev-colour)
                    (= 'white next-colour))
              0
              (count (take-while (partial not= (g next-colour))
                                 (concat (drop-while (partial not= (g prev-colour)) l)
                                         (take-while (partial not= (g prev-colour)) l))))))
        hue-change (f hue-cycle second)
        lightness-change (f lightness-cycle first)]
    (get-in commands [hue-change lightness-change])))

(defn piet-interpreter [image codel-size]
  (let [codel-map (grab-pixels image codel-size)]
    (loop [m (struct piet-machine pointer-cycle chooser-cycle nil [] "")
           [x y] [0 0]
           toggle 1]
      (if (<= toggle 8)
        (let [codel-block (find-colour-block-in codel-map x y)
              new-codel (choose-codel codel-block (first (:dp m)) (first (:cc m)))]
          (if-let [next-colour (get-in codel-map new-codel)]
            (let [command (piet-command (get-in codel-map [x y])
                                        next-colour)
                  machine (command (assoc m :value (count codel-block)))]
              (println (format "\n[%s %s] | %s" x y new-codel))
              (println (format "prev:%s, next:%s" (get-in codel-map [x y]) next-colour))
              (println (format "command: %s" (:name (meta command))))
              (println (format "machine: %s" machine))
              (recur machine new-codel 1))
            (if (zero? (clojure.core/mod toggle 2))
              (let [machine (do (update-in m [:dp] (partial rotate 1)))]
                (do  (println (format "\n[%s %s] | %s" x y new-codel))
                     (println (format "command: rotate_dp toggle: %s" toggle))
                     (println "machine: " machine)
                     (recur machine [x y] (inc toggle))))
              (let [machine (do (update-in m [:cc] (partial rotate 1)))]
                (do  (println (format "\n[%s %s] | %s" x y new-codel))
                     (println (format "command: rotate_cc toggle: %s" toggle))
                     (println "machine: " machine)
                     (recur machine [x y] (inc toggle)))))))
        (println (format "\nOutput: %s" (:out m)))))))

(defn -main [image codel-size]
  (piet-interpreter image (read-string codel-size)))