(ns #^{:author "James Cunningham"
       :doc "Deriving the formula for the area of a circle."}
  genetic.examples.circle
  (:use [genetic.code :only [bag pprint-code-structure code-structure-to-fn]]
        [genetic.population :only [evolve generate-population-ramped]]
        [genetic.nodes :only [arithmetic-bag make-ephemeral-number-node]]
        [genetic.utils :only [best]]))

(defn area [x] (* Math/PI x x))

(defn test-fitness-fn
  [individual]
  (reduce + (map #(Math/abs (- (individual %) (area %))) (range 5))))

(def ^:dynamic *last-run* (atom nil))

(defn pi-estimation
  [] (let [prog (code-structure-to-fn ;; (:top-individual (first @*last-run*))
                 (:top-individual (best #(/ 1 (:top-fitness %)) @*last-run*)))]
       (println "as fraction " (prog 1))
       (println "as decimal " (prog 1.0))))

(defn run-test
  [generations size]
  (pprint-code-structure
   (:top-individual
    (first (swap! *last-run*
                  (fn [_]
                    (evolve test-fitness-fn generations
                            (generate-population-ramped
                             [Object] Object
                             3 (concat (bag (make-ephemeral-number-node 0 10))
                                       arithmetic-bag)
                             size)))))))
  (pi-estimation))
