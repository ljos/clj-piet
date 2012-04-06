(ns clj-piet.core
  (:import javax.imageio.ImageIO)
  (:import java.io.File)
  (:import java.awt.Color))



(def pointer-cycle '(right down left up))
(def chooser-cycle '(left right))
(def lightness-cycle '(light normal dark))
(def hue-cycle '(red yellow green cyan blue magenta))

(defstruct piet-machine :dp :cc :value :stack)


(def colours
  (hash-map [0xFF 0xC0 0xC0] '(light red)
            [0xFF 0xFF 0xC0] '(light yellow)
            [0xC0 0xFF 0xC0] '(light green)
            [0xC0 0xFF 0xFF] '(light cyan)
            [0xC0 0xC0 0xFF] '(light blue)
            [0xFF 0xC0 0xFF] '(light magenta)
            [0xFF 0x00 0x00] '(normal red)
            [0xFF 0xFF 0x00] '(normal yellow)
            [0x00 0xFF 0x00] '(normal green)
            [0x00 0xFF 0xFF] '(normal cyan)
            [0x00 0x00 0xFF] '(normal blue)
            [0xFF 0x00 0xFF] '(normal magenta)
            [0xC0 0x00 0x00] '(dark red)
            [0xC0 0xC0 0x00] '(dark yellow)
            [0x00 0xC0 0x00] '(dark green)
            [0x00 0xC0 0xC0] '(dark cyan)
            [0x00 0x00 0xC0] '(dark blue)
            [0xC0 0x00 0xC0] '(dark magenta)
            [0xFF 0xFF 0xFF] '(nil white)
            [0x00 0x00 0x00] '(nil black)))

(defn grab-pixels [image codel-size]
  (let [img (ImageIO/read (File. image))]
    (println (.getWidth img) (.getHeight img))
    (for [y  (range (/ (.getHeight img) codel-size))]
      (for [x (range (/ (.getWidth img) codel-size))]
        (let [colour (Color. (.getRGB img (* x codel-size) (* y codel-size)))]
          (get colours [(.getRed colour) (.getGreen colour) (.getBlue colour)]))))))

(defn piet-interpreter [image codel-size]
  (let [codel-map (grab-pixels image codel-size)]
    (struct piet-machine pointer-cycle chooser-cycle nil '())))

(defn piet-pop [m]
  (update-in (update-in m [:value]
               (partial cons (peek (:stack m))))
    [:stack] pop))

(defn piet-push [m]
  (update-in (update-in m [:stack]
               (comp flatten (partial cons (first (:value m)))))
             [:value]
             empty))

(defn piet-update [f m]
  (piet-push (update-in m [:value]
               (comp list (partial apply f)))))

(defn piet-add [m]
  (piet-update + (piet-pop (piet-pop  m))))

(defn piet-subtract [m]
  (piet-update - (piet-pop (piet-pop m))))

(defn piet-multiply [m]
  (piet-update * (piet-pop (piet-pop m))))

(defn piet-divide [m]
  (piet-update / (piet-pop (piet-pop m))))

(defn piet-mod [m]
  (piet-update mod (piet-pop (piet-pop m))))

(defn piet-not [m]
  (piet-update #(if (zero? %) 1 0)
               (piet-pop m)))

(defn piet-greater [m]
  (piet-update #(if (< %1 %2) 1 0)
               (piet-pop (piet-pop m))))

(defn rotate [n coll]
  (if (zero? n)
    coll
    (if (pos? n)
      (rotate (dec n)
              (concat (rest coll)
                      (list (first coll))))
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
  (piet-update (partial repeat 2) (piet-pop m)))

(defn piet-roll [m]
  (update-in
   (let [machine (piet-pop (piet-pop m))]
     (assoc machine
       :stack (concat (rotate (second (:value machine))
                              (take (first (:value machine))
                                    (:stack machine)))
                      (drop (first (:value machine))
                            (:stack machine)))))
   [:value] empty))

(defn piet-in-char [m]
  (piet-update (partial list (int (first (read-line)))) m))

(defn piet-in-number [m]
  (piet-update (partial list (eval (read-string (read-line)))) m))

(defn piet-out-char [m]
  (let [machine (piet-pop m)]
    (print (char (first (:value machine))))
    (update-in machine [:value] empty)))

(defn piet-out-number [m]
  (let [machine (piet-pop m)]
    (print (first (:value machine)))
    (update-in machine [:value] empty)))
