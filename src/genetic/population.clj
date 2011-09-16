(ns #^{:author "James Cunningham"
       :doc "Library for managing populations of individuals."}
  genetic.population
  (:use [genetic.code :only (random-code-structure code-structure-to-fn)]
        [genetic.evolution :only (cross-over)]))

(defn generate-population-full
  "Generates a population using the full method - all individuals."
  [parameter-types return-type depth bag number]
  (repeatedly number
              (fn [] (random-code-structure bag #(= % depth)
                                            parameter-types return-type))))

(defn generate-population-grow
  "Generates a population using a version of the grow method"
  [parameter-types return-type max-depth bag number]
  (repeatedly number
              (fn [] (random-code-structure
                      bag #(and (>= % 2) (or (>= % max-depth) (<= (rand) 0.5)))
                      parameter-types return-type))))

(defn- divvy-up
  [total slots] (let [n (int (/ total slots))
                      r (rem total slots)]
                  (shuffle (concat (repeat (- slots r) n)
                                   (repeat r (inc n))))))

(defn generate-population-ramped
  "Generate a population using the ramped half-and-half method."
  [parameter-types return-type max-depth bag number]
  (apply concat
         (map (fn [number depth]
                (concat (generate-population-full
                         parameter-types return-type depth bag
                         (Math/floor (/ number 2)))
                        (generate-population-grow
                         parameter-types return-type depth bag
                         (Math/ceil (/ number 2)))))
              (divvy-up number (dec max-depth)) (iterate inc 2))))

(defn standard-evolution
  [population]
  (let [tournament-group (vec (take (Math/round (* 0.2 (count population)))
                                    population))
        best-group (map first (take (Math/round (* 0.01 (count population)))
                                     population))
        rest-size (- (count population) (count best-group))]
    (concat best-group
            (repeatedly rest-size
                        #(cross-over (first (rand-nth tournament-group))
                                     (first (rand-nth tournament-group)))))))

(defn evolve-generation
  ([fitness-fn evolution-fn population]
     (let [fitnesses (sort-by #(% 1)
                              (pmap (fn [i]
                                      [i (fitness-fn (code-structure-to-fn i))])
                                    population))]
       (evolution-fn fitnesses)))
  ([fitness-fn population]
     (evolve-generation fitness-fn standard-evolution population)))

