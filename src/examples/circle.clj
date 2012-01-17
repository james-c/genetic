(ns #^{:author "James Cunningham"
       :doc "Deriving the formula for the area of a circle."}
  examples.circle
  (:use [genetic.code :only [bag pprint-code-structure]]
        [genetic.population :only [evolve generate-population-ramped]]))

(defn #^{:tag Number :ephemeral? true :weight 10} a-number []
  (rand-int 10))

(defn plus [a b] (+ a b))

(defn times [a b] (* a b))

(defn safe-div [x y] (if (= y 0) 0 (/ x y)))

(defn area [x] (* Math/PI x x))

(defn test-fitness-fn
  [individual]
  (reduce + (map #(Math/abs (- (individual %)  (area %))) (range 10))))

(def ^:dynamic *last-run* (atom nil))

(defn run-test
  [generations size]
  (pprint-code-structure
   (:top-individual
    (first (swap! *last-run*
                  (fn [_]
                    (evolve test-fitness-fn generations
                            (generate-population-ramped
                             [Object] Object
                             3 (bag safe-div plus times a-number)
                             size))))))))