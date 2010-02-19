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



(defmacro save-binding
  "Saves the values of vars. Within body, you can wrap forms within 
'reload-binding', which will rebind the vars to the saved values.

This is particularly helpful when you create new threads which reset
the value of vars to their root values.

Example:

(def *a* nil)
(def *b* nil)

(defn foo []
  [*b* *a*])

(deref
 (binding [*a* 10
           *b* 20]
   (save-binding [*a* *b*]
     (future (reload-binding (foo))))))
=> [20 10]
"
  [[& vars] & body]
  (let [gensyms (map (fn [_] (gensym)) vars)]
    `(let ~(vec (mapcat (fn [v g] [g v]) vars gensyms))
       (macrolet [(~(symbol 'reload-binding) [& rest#]
                    `(binding ~~(vec (mapcat (fn [g v] [`'~v `'~g]) gensyms vars))
                       ~@rest#))]
          ~@body))))
