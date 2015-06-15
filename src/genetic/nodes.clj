(ns #^{:author "James Cunningham"
       :doc "Some common nodes"}
  genetic.nodes
  (:use [genetic.code :only (bag)]))

(defmacro def-tagged
  "Returns a function taking parameters tagged with types
   in ins, itself tagged with out, of name name, the body
   of which calls f with the given parameters."
  [name out ins f]
  (let [args (map #(with-meta (gensym) {:tag %}) ins)]
    `(defn ~(with-meta name {:tag out
                             :arglists `'~(list (vec args))
                             :name (str name)})
       ~(vec args) (~f ~@args))))


;; arithmetic
(def-tagged plus Number [Number Number] +)
(def-tagged times Number [Number Number] *)
(def-tagged minus Number [Number Number] -)
(def-tagged div Number [Number Number] #(if (zero? %2) 1 (/ %1 %2)))
;;(def-tagged increment Number [Number] inc)
(def arithmetic-bag (bag plus minus times div))

;; ephemeral random number
;; (the number chosen for a node is random, but stays the same)
(defmacro make-ephemeral-number-node [min max]
  `(with-meta (fn [] (+ ~min (rand-int ~(- max min))))
     {:tag Number :ephemeral? true :weight ~(- max min) :arglists (list [])}))

;; boolean
