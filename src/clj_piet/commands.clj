(ns clj-piet.commands)

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