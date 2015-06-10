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
  [weight-fn coll]
  (let [weights (reductions + (map weight-fn coll))
        choice (rand (last weights))]
    (ffirst (drop-while #(< (second %) choice)
                        (map #(list %1 %2) coll weights)))))
