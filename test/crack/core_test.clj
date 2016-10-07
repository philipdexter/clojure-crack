(ns crack.core-test
  (:require [clojure.test :refer :all]
            [crack.core :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def gen-range
  (gen/let [a gen/nat
            b gen/nat]
    (list (min a b) (max a b))))

(defn redu
  [n c]
  (if (= 0 n)
    (gen/return c)
    (gen/let [r gen-range]
      (redu (- n 1) (first (query (first r) (second r) c))))))

(def gen-cracked-and-vec
  (gen/let [num-queries (gen/large-integer* {:min 0, :max 10})
            xs (gen/list gen/nat)]
    (gen/fmap #(list % xs) (redu num-queries (from-list xs)))))

(def gen-cracked
  (gen/fmap first gen-cracked-and-vec))

(defn thing
  []
  (gen/sample gen-cracked))

(defn get-elems
  [c]
  ((comp sort to-list) c))

(defspec cracked-list-roundtrip
  (prop/for-all [c gen-cracked]
                (= (get-elems c) (get-elems ((comp from-list to-list) c)))))

(defspec exclusive-range
  (prop/for-all [c gen-cracked
                 i gen/nat]
                (= true (empty? (second (query i (inc i) c))))))

(defspec all-in
  (prop/for-all [cxs gen-cracked-and-vec]
                (let [c (first cxs)
                      xs (second cxs)
                      clist (to-list c)]
                  (= true (reduce #(and %1 %2) true (map #((comp not nil?) (some #{%} clist)) xs))))))

(defspec range-prop
  (prop/for-all [c gen-cracked
                 r gen-range]
                (let [low (first r)
                      high (second r)]
                  (= true (reduce #(and %1 %2) true (map #(and (> % low) (< % high)) (second (query low high c))))))))

(defspec all-queried-in
  (prop/for-all [c gen-cracked
                 r gen-range]
                (let [low (first r)
                      high (second r)
                      qd (query low high c)
                      c' (first qd)
                      q (second qd)]
                  (= true (reduce #(and %1 %2) true (map #(and (elem % c) (elem % c')) q))))))

(defspec all-in-after-query
  (prop/for-all [c gen-cracked
                 r gen-range]
                (let [low (first r)
                      high (second r)
                      qd (query low high c)
                      c' (first qd)]
                  (= (get-elems c) (get-elems c')))))
