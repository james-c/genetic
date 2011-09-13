(ns #^{:author "James Cunningham"
       :doc "Functions for producing random typed code structures."}
    genetic.code
    (:use [clojure.contrib.def :only (defn-memo)]
          [genetic.utils :only (weighted-choice metadata)]))

(defn-memo wrap-value
  ([value meta] (with-meta (fn [] value) (assoc meta :arglists (list []))))
  ([value] (wrap-value value {})))

(defn node-type
  [item] (or (:tag (metadata item)) Object))

(defn weight [node] (or (:weight (metadata node)) 1))

(defn ephemeral? [node] (:ephemeral? (metadata node)))

(defmacro def-ephemeral
  [name argslist & body]
  `(defn ~name {:ephemeral? true} ~argslist ~@body))

(defn reify-ephemeral
  [node] (if (not (ephemeral? node)) node (wrap-value (node) {:tag (node-type node)})))

(defn bag
  [& nodes] (map #(if (not (fn? %)) (wrap-value %) %) nodes))

(defn pick-from-bag
  [bag] (weighted-choice weight bag))

(defn terminals
  "Returns a collection of terminal nodes from a bag."
  ([type bag] (filter #(and (some empty? (:arglists (metadata %))) (isa? (node-type %) type)) bag))
  ([bag] (terminals Object bag)))

(defn- args-filter [args] (and (not-empty args) (not-any? #(= % '&) args)))

(defn internals
  "Returns a collection of internal nodes from a bag."
  ([type bag] (filter #(and (some args-filter (:arglists (metadata %))) (isa? (node-type %) type) bag)))
  ([bag] (internals Object bag)))