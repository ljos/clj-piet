(ns clj-piet.core
  (:import javax.imageio.ImageIO)
  (:import java.io.File)
  (:import java.awt.Color))

(def ^:private stack (atom '(2 2 2 2 2 2 2 2 1 1 3)))
(def ^:private functions (atom {}))
(def ^:private pointer (atom '(right down left up)))
(def ^:private switch (atom '(left right)))

(def lightness-cycle '(light normal dark))
(def hue-cycle '(red yellow green cyan blue magenta))


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
    codel-map))


(defmacro call-piet
  ([f]
     `((get @functions '~f)))
  ([f n]
     `((get @functions '~f) ~n)))

(defmacro def-piet [name args & body]
  `(swap! functions
          assoc
          '~name
          ~(case name
             pop `(fn []
                    (let [before# @stack
                          answer# (do ~@body)]
                      (println (format "%s %s = %s"
                                       '~name
                                       (if (empty? before#) "()" before#)
                                       answer#))
                      answer#))
             push `(fn ~args
                     (let [answer# (do ~@body)]
                       (println (format "%s %s = %s" '~name ~args answer#))
                       answer#))
             (pointer switch )
             ,`(fn []
                 (let ~(vec (interleave args (cycle [`(call-piet ~'pop)])))
                   (let [before# @~name
                         answer# (do ~@body)]
                     (println (format "%s %s %s = %s" '~name ~args before# answer#))
                     answer#)))
             roll `(fn []
                     (let ~(vec (interleave args (cycle [`(call-piet ~'pop)])))
                       (let [before# @~name
                             answer# (do ~@body)]
                         (println (format "%s %s %s = %s" '~name ~args before# answer#))
                         answer#)))
             `(fn []
                (println (format "%s {" '~name))
                (let ~(vec (interleave args (cycle [`(call-piet ~'pop)])))
                  (let [answer# (call-piet ~'push (do ~@body))]
                    (print (format "} = %s\n" answer#))
                    answer#))))))

(def-piet pop []
  (let [f (first @stack)]
    (swap! stack rest)
    f))

(def-piet push [n]
  (swap! stack conj n))

(def-piet add [n m]
  (+ n m))

(def-piet subtract [n m]
  (- n m))

(def-piet multiply [n m]
  (* n m))

(def-piet divide [n m]
  (/ n m))

(def-piet mod [n m]
  (mod n m))

(def-piet not [n]
  (if (zero? n) 1 0))

(def-piet greater [n m]
  (if (< n m) 1 0))

(defn rotate [coll n]
  (let [[x y] (split-at (+ n (if (pos? n) 0 (count coll))) coll)]
    (concat y x)))

(def-piet pointer [n]
  (apply list (swap! pointer rotate n)))

(def-piet switch [n]
  (apply list (swap! switch rotate n)))

(def-piet duplicate [n]
  (call-piet push n)
  (identity n))

(def-piet roll [n m]
  (swap! stack
         #(concat (rotate (take n %) m)
                  (drop n %))))

(def-piet in-char []
  (first (read-line)))

(def-piet in-number []
  (eval (read-string (read-line))))

(def-piet out-char [n]
  ())

(def-piet out-number [n]
  ())
