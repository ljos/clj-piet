(ns clj-piet.core
  (:import javax.imageio.ImageIO)
  (:import java.io.File)
  (:import java.awt.Color)
  (:use [clojure.set :only (union)]))

(def pointer-cycle '(right down left up))
(def chooser-cycle '(left right))
(def lightness-cycle '(light normal dark))
(def hue-cycle '(red yellow green cyan blue magenta))

(defstruct piet-machine :dp :cc :value :stack)

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


(defn piet-pop [m]
  (update-in (update-in m [:value]
               (partial cons (peek (:stack m))))
    [:stack] pop))

(defn piet-push [m]
  (update-in (update-in m [:stack]
               (comp #(apply list %) flatten (partial cons (first (:value m)))))
    [:value]
    empty))

(defn piet-update [m f n]
  (piet-push (update-in m [:value]
               (comp list (partial apply f) (partial take n)))))

(defn piet-add [m]
  (piet-update (piet-pop (piet-pop m)) + 2))

(defn piet-subtract [m]
  (piet-update (piet-pop (piet-pop m)) - 2))

(defn piet-multiply [m]
  (piet-update (piet-pop (piet-pop m)) * 2))

(defn piet-divide [m]
  (piet-update (piet-pop (piet-pop m)) / 2))

(defn piet-mod [m]
  (piet-update (piet-pop (piet-pop m)) mod 2))

(defn piet-not
  ([m]
     (piet-update (piet-pop m) #(if (zero? %) 1 0) 1)))

(defn piet-greater [m]
  (piet-update (piet-pop (piet-pop m))
               #(if (< %1 %2) 1 0)
               2))

(defn rotate [n coll]
  (if (zero? n)
    coll
    (if (pos? n)
      (rotate (dec n) (take (count coll) (drop 1 (cycle coll))))
      (rotate (inc n)
              (cons (last coll)
                    (butlast coll))))))

(defn piet-pointer [m]
  (update-in (let [machine (piet-pop m)]
               (update-in machine [:dp]
                 (partial rotate (first (:value machine)))))
    [:value] empty))

(defn piet-switch [m]
  (update-in (let [machine (piet-pop m)]
               (update-in machine [:cc]
                 (partial rotate (first (:value machine)))))
    [:value] empty))

(defn piet-duplicate [m]
  (piet-update (piet-pop (update-in m [:value] empty))
               (partial repeat 2)
               1))

(defn piet-roll [m]
  (update-in
   (let [machine (piet-pop (piet-pop m))]
     (assoc machine
       :stack (apply list
                     (concat (rotate (second (:value machine))
                                     (take (first (:value machine))
                                           (:stack machine)))
                             (drop (first (:value machine))
                                   (:stack machine))))))
   [:value] empty))

(defn piet-in-char [m]
  (piet-update m (partial list (int (first (read-line)))) 1))

(defn piet-in-number [m]
  (piet-update m (partial list (eval (read-string (read-line)))) 1))

(defn piet-out-char [m]
  (let [machine (piet-pop m)]
    (println (format "OUT: %s "(char (first (:value machine)))))
    (update-in machine [:value] empty)))

(defn piet-out-number [m]
  (let [machine (piet-pop m)]
    (println (format "OUT: %s "(first (:value machine))))
    (update-in machine [:value] empty)))

(defn piet-nop [m]
  (identity m))

(defn grab-pixels [image codel-size]
  (let [img (ImageIO/read (File. image))]
    (vec
     (for [x  (range (/ (.getWidth img) codel-size))]
       (vec
        (for [y (range (/ (.getHeight img) codel-size))
              :let [colour (Color. (.getRGB img (* x codel-size) (* y codel-size)))
                    c (get colours [(.getRed colour) (.getGreen colour) (.getBlue colour)])]
              :when (not= c [0x00 0x00 0x00])]
          c))))))

(defn find-colour-block-in
  ([codel-map x y]
     (if-let [codel-colour (get-in codel-map [x y])]
       (if (= codel-colour 'white)
         #{[x y]}
         (find-colour-block-in codel-map codel-colour x y))
       #{}))
  ([codel-map codel-colour x y]
     (when (= codel-colour (get-in codel-map [x y]))
       (union #{[x y]} 
              (find-colour-block-in (assoc-in codel-map [x y] nil) codel-colour (inc x) y)
              (find-colour-block-in (assoc-in codel-map [x y] nil) codel-colour (dec x) y)
              (find-colour-block-in (assoc-in codel-map [x y] nil) codel-colour x (inc y))
              (find-colour-block-in (assoc-in codel-map [x y] nil) codel-colour x (dec y))))))

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
  (let [commands [[piet-nop piet-push piet-pop]
                  [piet-add piet-subtract piet-multiply]
                  [piet-divide piet-mod piet-not]
                  [piet-greater piet-pointer piet-switch]
                  [piet-duplicate piet-roll piet-in-number]
                  [piet-in-char piet-out-number piet-out-char]]
        f (fn [l g]
            (if (or (= 'white prev-colour)
                    (= 'white next-colour))
              0
              (count (take-while (partial not= (g next-colour))
                                 (concat (drop-while (partial not= (g prev-colour)) l)
                                         (take-while (partial not= (g prev-colour)) l))))))
        hue-change (f hue-cycle second)
        lightness-change (f lightness-cycle first)
        command (get-in commands [hue-change lightness-change])]
    command))

(defn piet-interpreter [image codel-size]
  (let [codel-map (grab-pixels image codel-size)]
    (loop [m (struct piet-machine pointer-cycle chooser-cycle nil '())
           [x y] [0 0]
           toggle 0]
      (if (<= toggle 8)
        (let [codel-block (find-colour-block-in codel-map x y)
              new-codel (choose-codel codel-block (first (:dp m)) (first (:cc m)))]
          (if-let [next-colour (get-in codel-map new-codel)]
            (let [command (piet-command (get-in codel-map [x y])
                                        next-colour)
                  machine (command
                           (update-in m [:value] #(do % (list (count codel-block)))))]
              (println (format "\nx:%s, y:%s | %s" x y new-codel))
              (println (format "prev:%s, next:%s" (get-in codel-map [x y]) next-colour))
              (println (format "command: %s" command))
              (println (format "machine: %s" machine))
              (recur machine new-codel 0))
            (if (zero? (mod toggle 2))
              (recur (update-in m [:dp] (partial rotate 1)) [x y] (inc toggle))
              (recur (update-in m [:cc] (partial rotate 1)) [x y] (inc toggle)))))
        (println "\nFINISHED")))))

(defn -main [image codel-size]
  (piet-interpreter image codel-size))