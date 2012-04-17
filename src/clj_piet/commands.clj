(ns clj-piet.commands)


(defn piet-push [m]
  (update-in m [:stack]
    (partial cons (:value m))))

(defn piet-update [m f n]
  (piet-push (update-in (assoc m
                     :value (apply f (reverse (take n (:stack m)))))
          [:stack] nthnext n)))

(defn piet-pop [m]
  (update-in m [:stack] next))

(defn piet-add [m]
  (piet-update m + 2))

(defn piet-subtract [m]
  (piet-update m - 2))

(defn piet-multiply [m]
  (piet-update m * 2))

(defn piet-divide [m]
  (piet-update m (comp int /) 2))

(defn piet-mod [m]
  (piet-update m clojure.core/mod 2))

(defn piet-not [m]
  (piet-update m #(if (zero? %) 1 0) 1))

(defn piet-greater [m]
  (piet-update m #(if (< %1 %2) 1 0) 2))

(defn rotate [n coll]
  (if (zero? n)
    coll
    (if (pos? n)
      (rotate (dec n) (take (count coll) (drop 1 (cycle coll))))
      (rotate (inc n) (cons (last coll) (butlast coll))))))

(defn piet-pointer [m]
  (update-in (update-in m [:dp]
               (partial rotate (first (:stack m))))
    [:stack] rest))

(defn piet-switch [m]
  (update-in (update-in m [:cc]
               (partial rotate (first (:stack m))))
    [:stack] rest))

(defn piet-duplicate [m]
  (piet-push (assoc m
          :value (first (:stack m)))))

(defn piet-roll [m]
  (let [[k n] (take 2 (:stack m))]
    (update-in (update-in m [:stack] nnext)
      [:stack]  (comp (partial apply concat)
                      (juxt (comp (partial rotate k)
                                  (partial take n))
                            (partial drop n))))))

(defn piet-in-char [m]
  (piet-push (assoc m :value (int (first (read-line))))))

(defn piet-in-number [m]
  (piet-push (assoc m :value (eval (read-string (read-line))))))

(defn piet-out-char [m]
  (update-in (update-in m
               [:out] str (char (first (:stack m))))
    [:stack] rest))

(defn piet-out-number [m]
  (update-in (update-in m
               [:out] str (first (:stack m)))
    [:stack] rest))

(defn piet-nop [m]
  (identity m))