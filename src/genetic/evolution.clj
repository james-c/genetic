(ns #^{:author "James Cunningham"
       :doc "Code structure evolution library."}
  genetic.evolution
  (:require [clojure.zip :as zip])
  (:use [genetic.code :only (node-type)]))

(defn- returns? [node type] (isa? (node-type node) type))

(defn- type-counter [type] #(if (returns? % type) 1 0))

(defn- is-internal? [node] (and (seq? node) (not-empty (rest node))))

(defn- code-struct-zipper
  [code-structure]
  (zip/zipper is-internal?
              (fn [node] (if (fn? (first node)) (rest node) node))
              (fn [node children]
                (with-meta
                  (if (fn? (first node)) (cons (first node) children) children)
                  (meta node)))
              code-structure))

(defn- get-node
  [loc]  
  (let [node (loc 0)]
    (if (and (seq? node) (fn? (first node))) (first node) node)))

(defn- collect-from-nodes
  [coll-fn cs]
  (loop [loc (code-struct-zipper cs)
         coll nil]
    (if (zip/end? loc) coll
        (recur (zip/next loc) (cons (coll-fn (get-node loc)) coll)))))

(defn- depth
  [cs] (letfn [(dep [d cs]
                 (if (not (is-internal? cs))
                   d (reduce max (map #(dep (inc d) %) (rest cs)))))]
         (dep 1 cs)))

(defn- types [cs] (seq (set (collect-from-nodes node-type cs))))

(defn- node-count
  ([type cs] (reduce + (collect-from-nodes (type-counter type) cs)))
  ([cs] (node-count Object cs)))

(defn do-to-nth-node
  ([count? f n cs]
      (loop [loc (code-struct-zipper cs)
             count 0]
        (if (or (= count n) (zip/end? loc))
          (f loc)
          (recur (zip/next loc) (if (count? loc) (inc count) count)))))
  ([f n cs] (do-to-nth-node (fn [_] true) f n cs)))

(defn replace-nth-node
  ([count? f n cs]
      (do-to-nth-node
       count? #(zip/root (zip/replace % (f (zip/node %)))) n cs))
  ([f n cs] (replace-nth-node (fn [_] true) f n cs)))

(defn get-nth-node
  ([count? n cs] (do-to-nth-node count? zip/node n cs))
  ([n cs] (do-to-nth-node zip/node n cs)))

(defn mutator [node] (or (:mutator (meta node)) identity))

(defn mutate
  "Return a structure produced from code-structure with a random
   node changed through the application of mutator to that node.
   Uses mutator function if non given."
  ([mutator code-structure]
      (replace-nth-node (fn [loc] (with-meta (mutator loc) (meta loc)))
                        (rand-int (node-count code-structure))
                        code-structure))
  ([code-structure] (mutate #((mutator %) %) code-structure)))

(defn cross-over
  "Return a structure that is the result of replacing a node in m
   with a node from f of the same type."
  ([max-depth m f]
     (let [m-node-type (rand-nth (types m)) ;; should select node not type
           m-node (get-nth-node (type-counter m-node-type)
                                (rand-int (node-count m-node-type m))
                                m)
           f-node-count (node-count m-node-type f)]
       (if (= f-node-count 0) f
           (let [child (replace-nth-node (type-counter type)
                                         #(with-meta m-node (meta %))
                                         (rand-int f-node-count)
                                         f)]
             (if (and max-depth (> (depth child) max-depth))
               (rand-nth [m f]) child)))))
  ([m f] (cross-over nil m f)))

;; utils

(defn maximum-depth
  "Convenience function for returing a simple max depth function"
  [max] (fn [depth] (= depth max)))

(defn variable-depth
  "Returns a function for use in mixed depth tree generation
   max - maximum depth
   p-terminal probability of outputing a terminal node"
  [max p-terminal]
  (fn [depth] (and (>= depth 2) (or (>= depth max)
                                    (<= (rand) p-terminal)))))

