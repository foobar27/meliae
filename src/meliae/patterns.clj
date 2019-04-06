(ns meliae.patterns
  (:require [meliae.utils :refer [flat-for check-not-nil make-symbol make-local-keyword make-local-symbol resolve-symbol]]
            [clojure.spec.alpha :as s]
            [clojure.core.match :refer [match]]
            [riddley.walk :refer [macroexpand-all walk-exprs]]
            [clojure.pprint :refer [pprint simple-dispatch]]
            [clojure.string :refer [capitalize]]))

;; TODO is it possible to keep the line numbers for improved error messages?

(defonce ^:private matcher-form-registry (atom {}))

(defn register-matcher-form! [k v]
  (if (qualified-symbol? k)
    (swap! matcher-form-registry assoc k v)
    (throw (IllegalArgumentException. (str "Tried to register a matcher form of type " (type k) ":" k)))))

(defn get-matcher-form [k]
  (if (qualified-symbol? k)
    (check-not-nil (-> @matcher-form-registry (get k))
                   "Could not find matcher-form for parent, looking for "
                   k
                   " in : "
                   (keys @matcher-form-registry))
    (throw (IllegalArgumentException. (str "Tried to do a matcher-form registry lookup with type " (type k) ":" k)))))

(defmulti print-pattern class :default ::classic-pprint)
(defmethod print-pattern ::classic-pprint [v]
  (if (sequential? v)
    (do
      (print (if (vector? v) "[" "("))
      (doseq [[i x] (map vector (iterate inc 0) v)]
        (when (pos? i)
          (print " "))
        (print-pattern x))
      (print (if (vector? v) "]" ")")))
    (print v)))

;; TODO Define PARENT patterns
;; TODO this is not really generic, and also not composable (parents cannot be parents, which would be useful for n-ary operators)
(defmacro defmultipattern [symbol]
  (let [spec-symbol (make-symbol symbol "-spec")
        record-symbol (-> symbol name capitalize make-symbol)
        record-ctor-symbol (make-symbol "->" record-symbol)
        kw (make-local-keyword symbol)
        ctor-symbol (make-symbol "->" symbol)
        pprint-symbol (make-symbol "pprint-" symbol)]
    `(do
       (defmulti ~spec-symbol ::kind) ;; TODO hardcoded ::kind
       (s/def ~kw (s/multi-spec ~spec-symbol ::kind))
       (register-matcher-form! '~(make-local-symbol ctor-symbol)
                               (fn [kind#]
                                 {::kind kind#}))
       (s/fdef ~ctor-symbol
         :args (s/cat :kind qualified-keyword?
                      :args (s/* (s/cat :kw (s/and qualified-keyword?
                                                   #(not (= % ::kind)))
                                        :value any?)))
         :ret any?)
       ;; TODO use name-with-attributes (does that include spec docs?)
       (defrecord ~record-symbol []) ;; this is mainly for dispatching pprint
       (defmulti ~pprint-symbol ::kind)
       (defmethod print-pattern ~record-symbol [v#]
         (~pprint-symbol v#))
       (defn ~ctor-symbol [kind# & args#]
         (reduce (fn [r# [k# v#]] (assoc r# k# v#))
                 (assoc (~record-ctor-symbol) ::kind kind#)
                 (partition 2 args#))))))

;; TODO proper indentation for flat-for

;; TODO test parent and child in different namespaces (required via :as)
;; TODO also test pattern matching done in yet another namespace

;; TODO use name-with-attributes helper from tools.macro => does that include specs?
(s/fdef defpattern
  :args (s/cat :parent-symbol symbol?
               :symbol simple-symbol?
               :named-args (s/and vector?
                                  (s/* (s/cat :binding simple-symbol?
                                              :body    any?)))))
(defmacro defpattern [parent-symbol symbol named-args]
  (let [ctor-symbol        (make-symbol "->" symbol)
        q-symbol           (make-symbol symbol "?")
        qx-symbol          (gensym "qx")
        parent-ctor-symbol (make-symbol "->" parent-symbol)
        resolved-parent-ctor-symbol (check-not-nil (resolve-symbol parent-ctor-symbol)
                                                   "Undefined symbol: " parent-ctor-symbol " (did you call defmultipattern?)")
        parent-spec-symbol (make-symbol parent-symbol "-spec")
        pprint-symbol (make-symbol "pprint-" parent-symbol)
        kw (make-local-keyword symbol)
        named-args (partition 2 named-args)
        arg-symbols (map first named-args)
        arg-kws (map make-local-keyword arg-symbols)
        parent-ctor-args (flat-for [s arg-symbols]
                             [(make-local-keyword s) s])
        parent-matcher-form (get-matcher-form resolved-parent-ctor-symbol)] 
    `(do
       (defmethod ~parent-spec-symbol ~kw [_#]
         ;; We would need something to verify keys and their corresponding values here.
         ;; Let's just assume for now that everything has been constructed via the right ctor
         ;; (and is thus guaranteed to be correct)
         any?)
       (register-matcher-form! '~(make-local-symbol ctor-symbol)
                               (fn [~@arg-symbols]
                                 (cond-> (~parent-matcher-form ~kw)
                                   ~(if (seq arg-symbols) true)
                                   (assoc 
                                    ~@(flat-for [s arg-symbols]
                                          [(make-local-keyword s) s])))))
       (defmethod ~pprint-symbol ~kw [v#]
         (.write ^java.io.Writer *out* "(")
         (.write ^java.io.Writer *out* ~(name ctor-symbol))
         (doseq [kw# [~@arg-kws]
                 :let [v# (get v# kw#)]]
           (.write ^java.io.Writer *out* " ")
           (print-pattern v#))
         (.write ^java.io.Writer *out* ")"))
       (defn ~ctor-symbol [~@arg-symbols]
         (~parent-ctor-symbol ~kw ~@parent-ctor-args))
       (defn ~q-symbol [~qx-symbol]
         (and (= ~kw (get ~qx-symbol ::kind)) ;; TODO hardcoded ::kind
              ~@(for [kw arg-kws]
                  `(contains? ~qx-symbol ~kw)))))))

(defmacro defpatterns [parent-symbol & patterns]
  `(do
     ~@(for [[symbol named-args] (partition 2 patterns)]
         `(defpattern ~parent-symbol ~symbol ~named-args))))

;;
;; matching macros
;;

(defmacro matcher-form-call [& args]
  (do
    (let [matcher-form (-> &form first meta ::matcher-form)]
      (apply matcher-form args))))

(defn- introduce-matcher-form [s]
  (if (symbol? s)
    (if-let [matcher-form (get @matcher-form-registry (resolve-symbol s))]
      (vary-meta `matcher-form-call
                 assoc ::matcher-form matcher-form))))

(defn- inline-patterns [expr]
  (macroexpand-all (walk-exprs introduce-matcher-form introduce-matcher-form expr)))

(defmacro match* [vars & clauses]
  (let [vars-s (gensym "vars")]
    `(let [~vars-s ~vars]
       (match ~vars-s
         ~@(flat-for [[lhs rhs] (partition 2 clauses)
                      :let [lhs (if (sequential? lhs)
                                  (vec (map inline-patterns lhs))
                                  lhs)]]
               [lhs rhs])
         ~@(if (and (not (some #{:else} (map first (partition 2 clauses)))))
             `[:else (throw (IllegalArgumentException.
                             (str "No matching clause: "
                                  (with-out-str (meliae.patterns/print-pattern ~vars-s)))))]
             ;; there is already an :else clause
             [])))))

