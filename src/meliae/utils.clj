(ns meliae.utils)

(defmacro flat-for [args & body]
  `(apply concat (for [~@args] ~@body)))

(defmacro check-not-nil [x & args]
  `(or ~x (throw (IllegalArgumentException. (str ~@args)))))

(defn make-local-keyword [kw]
  (let [ns (str *ns*)]
    (keyword ns (str kw))))

(defn make-symbol [& args]
  (symbol (apply str args)))

(defn make-local-symbol [& args]
  (let [ns (str *ns*)]
    (symbol ns (apply str args))))

(defn var->sym [v]
  (symbol (str (.-ns v))
          (str (.-sym v))))

(defn resolve-symbol [s]
  (some-> s resolve var->sym))

