(ns crack.core)

(require '[clojure.core.match :refer [match]])

(defn partition
  "split a vector in two based on a predicate"
  [pred vector]
  ((juxt (comp vec filter) (comp vec remove)) pred vector))

(defn empty
  "create an empty crack"
  [] '())

(defn to-list
  "convert a crack to a list"
  [crack]
  (match [crack]
         [(v :guard vector? v)] (seq v)
         [([_ & ([l & ([r & _] :seq)] :seq)] :seq)] (into [] (concat (to-list l) (to-list r)))
         :else "error: to-list: match"))

(defn from-list
  "create a crack from a list"
  [list]
  (into [] list))

(defn length
  "get the number of elements in a crack"
  [crack]
  (match [crack]
         [(v :guard vector? v)] (count v)
         [([_ & ([l & ([r & _] :seq)] :seq)] :seq)] (+ (length l) (length r))
         :else "error: length: match"))

(defn null?
  "check if the crack is empty"
  [crack]
  (match [crack]
         [(v :guard vector? v)] (empty? v)
         [([_ & ([l & ([r & _] :seq)] :seq)] :seq)] (and (null? l) (null? r))
         :else "error: null?: match"))

(defn query
  "query for values between low and high"
  [low high crack]
  (match [crack]
         [(v :guard vector? v)]
         (let [splits (partition #(<= % low) v)
               lower (first splits)
               higher (second splits)
               splits' (partition #(< % high) higher)
               middle (first splits')
               highest (second splits')
               crack' (list low lower higher)]
           (list crack' middle))
         [([le & ([vlow & ([vhigh & _] :seq)] :seq)] :seq)]
         (let [splits (if (or (> low le) (> high le))
                        (query low high vhigh)
                        (list vhigh []))
               down-right (first splits)
               right-res (second splits)
               splits' (if (> low le)
                         (list vlow [])
                         (query low high vlow))
               down-left (first splits')
               left-res (second splits')]
               (list
                (list le down-left down-right)
                (into [] (concat left-res right-res))))
         :else "error: query: match"))

(defn elem
  "checks for membership of a single item in a crack"
  [i crack]
  ((comp not empty? second) (query (dec i) (inc i) crack)))

; todo optimize so don't get
; ((1 (0 [] [1]) (0 [] [2 34 5 67 2 7 8 3 2])) [1 2 34 5 2 7 8 3 2])
; (on right branch don't need the 0 since nothing greater than 1 can be less than 0

; todo
; use sized

; todo
; add combinators to clojure quickcheck
