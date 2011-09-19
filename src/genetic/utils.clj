(ns #^{:author "James Cunningham"
       :doc "Genetic programming utility functions."}
    genetic.utils)

(defn weighted-choice
  "Return a random element of a collection with the probability
   of the element being returned being weighted by the application
   of weight-fn to that element."
  [weight-fn coll]
  (let [weights (reductions + (map weight-fn coll))
        choice (rand (last weights))]
    (ffirst (drop-while #(< (second %) choice)
                        (map #(list %1 %2) coll weights)))))

(defmacro metadata [n] `(let [name# (:name (meta ~n))
                              ns# (:ns (meta ~n))
                              v# (if name#
                                   (or (ns-resolve ns# (symbol name#)) ~n)
                                   ~n)]
                          (meta v#)))
