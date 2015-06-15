(ns #^{:author "James Cunningham"
       :doc "Genetic programming utility functions."}
  genetic.utils
  (:use [clojure.core.memoize :only (memo)]))

(defmacro def-memo
  [name arg-list & body]
  `(def ~name (memo (fn ~arg-list ~@body))))

(defn weighted-choice
  "Return a random element of a collection with the probability
   of the element being returned being weighted by the application
  of weight-fn to that element."
  ([weight-fn choices]
     (letfn [(f [[total choice] next]
               (let [weight (weight-fn next)
                     total (+ total weight)]
                 [total (if (< (rand) (/ weight total)) next choice)]))]
       (second (reduce f [0 nil] choices))))
  ([choices] (weighted-choice (fn [_] 1) choices)))

(defn best
  ([f seq]
   (second (reduce (fn [[v i] next] (let [val (f next)]
                                      (if (> val v)
                                        [val next]
                                         [v i])))
                   [Double/NEGATIVE_INFINITY nil] seq)))
  ([n f seq]
   (if (<= n 0) []
       (letfn [(b [vals next]
                 (let [val (f next)]
                   (if (< (count vals) n)
                     (sort-by first (conj vals [val next]))
                     (if (> val (ffirst vals))
                       (sort-by first (conj (rest vals) [val next]))
                       vals))))]
         (map second (reduce b [] seq))))))

(defn divvy-up
  [total slots] (let [n (int (/ total slots))
                      r (rem total slots)]
                  (shuffle (concat (repeat (- slots r) n)
                                   (repeat r (inc n))))))
