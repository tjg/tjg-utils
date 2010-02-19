(ns tjg-utils
  #^{:author "Tayssir John Gabbour",
     :doc "Handy utils"}
  (:gen-class)
  (:use clojure.contrib.macro-utils))

(defmacro #^{:private true} assert-args [fnname & pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  ~(str fnname " requires " (second pairs)))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args fnname more)))))

(defmacro with-open*
  "bindings   => [name init ...] | [name [init close-form] ...]

  Evaluates body in a try expression with names bound to the values
  of the inits, and a finally clause that calls close-form on each
  name in reverse order.

  Acts just like with-open, except when 'bindings' looks like
  [init close-form]."
  [bindings & body]
  (assert-args with-open*
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  (cond
    (= (count bindings) 0) `(do ~@body)
    (and (symbol? (bindings 0))
         (vector? (bindings 1)))
    (let [[sym [init close-form]] (subvec bindings 0 2)]
      `(let [~sym ~init]
         (try
          (with-open* ~(subvec bindings 2) ~@body)
          (finally
           ~close-form))))
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (with-open* ~(subvec bindings 2) ~@body)
                                (finally
                                  (. ~(bindings 0) close))))
    :else (throw (IllegalArgumentException.
                   "with-open* only allows Symbols in bindings"))))

(defn- representation-of-vars->bindmap
  "Makes a sexp that, when evaluated, will create a binding-map for
  vars."
  [var-names]
  (let [var-forms (map (fn [v] `(var ~v))
                       var-names)]
    `(hash-map ~@(interleave var-forms var-names))))

(defmacro with-binding-map
  "Makes a map of vars and their current values. (The \"binding map\".) 
This map is then bound to name, as if by let.

Notes: 
  Particularly useful when you're making a chlid thread which should
  inherit current var bindings.

  The binding-map is useful for with-bindings.

Example:
  (def *a* nil)
  (def *b* nil)
  (deref
   (binding [*a* :new-binding, *b* :new-binding]
     (with-binding-map [foo *a* *b*]
       (future (with-bindings foo
                 [*a* *b*])))))
  => [:new-binding :new-binding]"
  [[name & vars] & body]
  (let [var-map (representation-of-vars->bindmap vars)]
    `(let [~name ~var-map]
       ~@body)))
