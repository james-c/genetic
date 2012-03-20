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

(defn select-by-tournament
  [n tournament-size selector seq]
  (take n (sort-by selector (take tournament-size (shuffle seq)))))

(defn standard-evolution
  [population]
  (let [tournament-size (Math/round (* 0.1 (count population)))
        best-group (map first (take (Math/round (* 0.001 (count population)))
                                    population))
        rest-size (- (count population) (count best-group))]
    (concat best-group
            (repeatedly rest-size
                        #(apply cross-over 20
                                (map first
                                     (select-by-tournament
                                      2 tournament-size
                                      second population)))))))

(defn assess-fitness
  [fitness-fn population]
  (sort-by second
           (pmap (fn [i] [i (try
                              (fitness-fn (code-structure-to-fn i))
                              (catch ArithmeticException e
                                Double/POSITIVE_INFINITY))])
                 population)))

(defrecord CensusInfo [size top-individual top-fitness average-fitness])

;; what if there are equally good best individuals?
(defn census
  [population]
  (let [size (count population)]
    (CensusInfo. size
                 (ffirst population)
                 (second (first population))
                 (/ (reduce + (map second population)) size))))

(defn record-census-info
  [n time info] (println "generation" n
                         "best" (:top-fitness info)
                         "avg" (:average-fitness info)
                         "time" time))

;; todo - time limited run
(defn evolve
  ([fitness-fn evolution-fn generations initial-population]
     (loop [population (assess-fitness fitness-fn initial-population)
            censuses (list (census population))
            count 1
            time (System/currentTimeMillis)]
       (let [new-population (assess-fitness fitness-fn
                                            (evolution-fn population))]         
         (record-census-info count
                             (- (System/currentTimeMillis) time)
                             (first censuses))
         (if (= count generations) censuses
             (recur new-population
                    (cons (census new-population) censuses)
                    (inc count)
                    (System/currentTimeMillis))))))
  ([fitness-fn generations initial-population]
     (evolve fitness-fn standard-evolution generations initial-population)))

