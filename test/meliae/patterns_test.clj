(ns meliae.patterns-test
  (:require [meliae.patterns :refer :all]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(defn- pattern->string [pattern]
  (with-out-str
    (print-pattern pattern)))

(stest/instrument (stest/enumerate-namespace 'meliae.patterns))

(s/def ::color #{::red ::green ::yellow ::black})

(defmultipattern fruit)
(defpatterns fruit
  apple [color ::color]
  constant []
  banana [color ::color sweetness integer?]
  basket [fruit1 ::fruit fruit2 ::fruit])

(defmultipattern recursive-basket)
(defpatterns recursive-basket
  baskets-basket [baskets (s/coll-of ::recursive-basket)]
  fruit-basket [fruits (s/coll-of ::fruit)])

;; Since all the patterns have been defined now, let's instrument them.
(stest/instrument (stest/enumerate-namespace 'meliae.patterns-test))

(deftest to-string
  (testing "simple hierarchy"
    (is (= (pattern->string (->basket (->banana ::yellow 10)
                                      (->apple ::green)))
           "(->basket (->banana :meliae.patterns-test/yellow 10) (->apple :meliae.patterns-test/green))")))
  (testing "constant to string"
    (is (= (pattern->string (->constant))
           "(->constant)")))
  (testing "recursive basket to string"
    (is (= (pattern->string (->baskets-basket
                             [(->fruit-basket (map identity [(->constant)
                                                             (->banana ::yellow 10)]))
                              (->fruit-basket [(->apple ::red)])]))
           "(->baskets-basket [(->fruit-basket ((->constant) (->banana :meliae.patterns-test/yellow 10))) (->fruit-basket [(->apple :meliae.patterns-test/red)])])"))))

(deftest matching
  (testing "basket"
    (let [u (->banana ::yellow 10)
          v (->banana ::black 12)]
      (is (= (match* [(->basket u v)]
               [(->basket a b)] {:a a :b b})
             {:a u :b v}))))
  (testing "zero argument constructor"
    (is (match* [(->constant)]
          [(->constant)] true))))

(deftest no-matching-clause
  (testing "default clause"
    (is (= (match* [(->apple ::red)]
             :else "No matching clause")
           "No matching clause")))
  (testing "missing clause should show a readable pattern"
    (is (thrown-with-msg? IllegalArgumentException #"\QNo matching clause: [(->apple :meliae.patterns-test/red)]\E"
                          (match* [(->apple ::red)]
                            [(->constant)] true)))))

(deftest predicates
  (testing "apples and bananas"
    (let [apple (->apple ::red)
          banana (->banana ::red 10)]
      (is (apple? apple))
      (is (not (apple? banana)))
      (is (banana? banana))
      (is (not (banana? apple))))))

(deftest spec-test
  (is (s/valid? ::fruit (->banana ::yellow 2)))
  (is (not (s/valid? ::fruit 2))))
