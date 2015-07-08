(ns #^{:author "James Cunningham"
       :doc "Functions for producing random typed code structures."}
    genetic.code
    (:use [clojure.core.memoize :only (memo)]
          [clojure.walk :only (postwalk)]
          [clojure.pprint :only (pprint with-pprint-dispatch code-dispatch)]
          [genetic.utils :only (weighted-choice def-memo)]))

(def-memo wrap-value
  ([value meta] (with-meta (fn [] value)
                  (assoc meta :arglists (list [])
                         :wrapped true
                         :tag (or (:tag meta) (type value)))))
  ([value] (wrap-value value {})))

(defn wrapped? [node] (:wrapped (meta node)))

(defn node-type
  [node] (or (:tag (meta node)) Object))

(def-memo eval-node-type
  [node] (eval (node-type node)))

(defn weight [node] (or (:weight (meta node)) 1))

(defn ephemeral? [node] (:ephemeral? (meta node)))

(defmacro def-ephemeral
  [name argslist & body]
  `(defn ~name {:ephemeral? true} ~argslist ~@body))

(defn reify-ephemeral
  [node]
  (if (not (ephemeral? node)) node (wrap-value (node) {:tag (node-type node)})))

(defmacro try-resolve
  [v] `(cond (symbol? '~v) (with-meta ~v (meta (resolve '~v)))
             (not (fn? ~v)) (wrap-value ~v)
             :else ~v))

(defmacro bag [& nodes] `(vec (list ~@(map #(list `try-resolve %) nodes))))

(defn pick-from-bag
  [bag] (weighted-choice weight bag))

(defn terminals
  "Returns a collection of terminal nodes from a bag."
  ([type bag] (filter #(or (not (fn? %))
                           (and (some empty? (:arglists (meta %)))
                                (isa? (node-type %) type)))
                      bag))
  ([bag] (terminals Object bag)))

(defn- args-filter [args] (and (not-empty args) (not-any? #(= % '&) args)))

(defn internals
  "Returns a collection of internal nodes from a bag."
  ([type bag] (filter #(and (some args-filter (:arglists (meta %)))
                            (isa? (node-type %) type))
                      bag))
  ([bag] (internals Object bag)))

(defn- construct-terminal
  [type bag]
  (let [terminal (reify-ephemeral (pick-from-bag (terminals type bag)))]
    (if (fn? terminal) (list terminal) terminal)))

(defn- construct-internal
  [arg-constructor type bag]
  (let [node (pick-from-bag (internals type bag))
        arglist (rand-nth (filter args-filter (:arglists (meta node))))]
    (cons node (map #(arg-constructor (eval-node-type %)) arglist))))

(defn- generate-code
  [bag is-terminal? current-depth type]
  (if (is-terminal? current-depth)
    (construct-terminal type bag)
    (construct-internal (partial generate-code bag is-terminal?
                                 (inc current-depth))
                        type bag)))

(defn- generate-parameters
  [parameter-types]
  (map #(with-meta (symbol (str "arg-" %2)) {:tag %1})
       parameter-types (iterate inc 1)))

(defn random-code-structure
  "Return a code structure constructed from nodes from bag."
  ([bag is-terminal? parameter-types return-type]
      (with-meta
        (generate-code (concat (generate-parameters parameter-types) bag)
                       is-terminal? 0 return-type)
        {:params parameter-types}))
  ([bag is-terminal parameter-number]
      (random-code-structure bag is-terminal
                             (repeat parameter-number Object) Object)))

(defn- wrap-symbol
  [cs]
  (let [arg-seq (map #(symbol (str "arg-" %)) (iterate inc 1))]
    (fn [& args]
      (let [bindings (apply hash-map (interleave arg-seq args))]
        (or (get bindings cs) cs)))))

(defn- call-fn
  [f b] (fn [& a] (apply f (map #(if (fn? %) (apply % a) %) b))))

(defn code-structure-to-fn
  "Create the function described by the given code-structure."
  [cs]
  (cond   
   (symbol? cs) (wrap-symbol cs)
   (and (seq? cs) (first cs)) (call-fn (first cs) (map code-structure-to-fn (rest cs)))
   :else cs))

(defn pprint-code-structure
  "Pretty print the function described by the code-structure."
  [cs]
  (with-pprint-dispatch #(code-dispatch (or (and (fn? %) (:name (meta %))) %))
    (pprint (list 'fn (vec (generate-parameters (:params (meta cs))))
                  (postwalk #(if (and (seq? %) (wrapped? (first %)))
                               ((first %)) %) cs)))))

