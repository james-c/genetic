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

