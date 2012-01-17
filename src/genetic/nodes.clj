(ns #^{:author "James Cunningham"
       :doc "Some common nodes"}
  genetic.nodes
  (:use [genetic.code :only (bag)]))

;; arithmetic
(defn #^{:tag Number} plus [#^Number x #^Number y] (+ x y))

(defn #^{:tag Number} minus [#^Number x #^Number y] (- x y))

(defn #^{:tag Number} times [#^Number x #^Number y] (* x y))

(defn #^{:tag Number} div [#^Number x #^Number y] (if (= y 0) 0 (/ x y)))

(def arithmatic-bag (bag plus minus times div))

(defmacro make-ephemeral-number-node [min max]
  `(with-meta (fn [] (+ ~min (rand-int ~(- max min))))
     {:tag Number :ephemeral? true :weight ~(- max min) :arglists (list [])}))