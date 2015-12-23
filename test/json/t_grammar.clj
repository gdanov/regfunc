(ns json.t-grammar
  (:require [json.grammar :as gr :refer :all :exclude [one-or-more]]
            [regfunc.macro :refer :all]
            [midje.sweet :refer :all :exclude [one-of anything]]
            [taoensso.timbre.profiling :as prof])
  (:import [java.io InputStreamReader ByteArrayInputStream FileInputStream BufferedReader]))

(facts "match"
  ((*eq 1) '(1)) => (->Token 1 '())
  ((*eq 1) '(1 2)) => (->Token 1 '(2))
  ((*eq 1) '(2)) => nil
  ((*eq 1) '()) => nil
  ((*eq 1) nil) => nil
  ((*eq 1) (->Token 1 '())) => nil)

(facts "repeat"
  ((*repeat (*eq 1)) '(1 1)) => (->T [(->T 1) (->T 1)])
  ((*repeat (*eq 1)) '(1 1 2)) => (->Token [(->T 1) (->T 1)] '(2))
  ((*repeat (*eq 1)) '(2 1)) => nil
  ((*repeat (*eq 1)) '()) => nil
  ((*repeat (*eq 1)) nil) => nil
  ;((*repeat (*group (*eq 1)))'(1 1)) => ^:repeat (->T [(->T 1) (->T 1 )])
  )

(fact "splice"
  ((*repeat (*splice (*group (*eq 1)))) '(1 1)) => ^:repeat (->T [^:eq (->T 1) ^:eq (->T 1)])
  ;; no effect without outer rule
  ((*splice (*group (*eq 1) (*eq 2))) '(1 2)) => (->T [(->T 1) (->T 2)])
  ;; now with the outer rule, the inner group is "transparent"
  ((*group (*splice (*group (*eq 1) (*eq 2)))) '(1 2)) => (->T [(->T 1) (->T 2)])

  ((*group (*splice (*zero-or-more (*splice (*eq 2))))) '(2)) => (->T [(->T 2)])) 

(facts "zero-or-more"
  ((*zero-or-more (*eq 1)) '(1 1)) => (->T [(->T 1) (->T 1)] '())
  ((*zero-or-more (*eq 1)) '(1 1 2)) => (->T [(->T 1) (->T 1)] '(2))
  ((*zero-or-more (*eq 2)) '(1 1)) => (->T nil '(1 1)) 
  ((*zero-or-more (*eq 1)) '()) => (->Token nil '())
  ((*zero-or-more (*eq 1)) nil) => (t nil)

  ((*zero-or-more (*group (*eq 1))) '(1)) => (->T [(->T 1)])
;  ((*zero-or-more (*splice (*group (*eq 1)))) '(1)) => (->T [(->T 1)])
  )

(facts "group"
  ((*group (*eq 1)) '(1))
  ((*group (*eq 1) (*eq 2)) '(1 2)) => (->T [(->T 1 ) (->T 2)])
  ((*group (*eq 1) (*eq 2)) '(1 2 3)) => (->T [(->T 1) (->T 2 )] '(3))
  ((*group (*eq 1) (*eq 2)) '(1 1 2)) => nil
  ((*group (*eq 1) (*eq 2)) '(1)) => nil
  ((*group (*eq 0) (*group (*eq 1) (*eq 2)) (*eq 3)) '(0 1 2 3)) =>
  (->T [(->T 0)
        (->T 1)
        (->T 2)
        (->T 3)] '())
  ((*group (*eq 0) (*splice (*group (*eq 1) (*eq 2))) (*eq 3)) '(0 1 2 3)) =>
  (->T [(->T 0)
        (->T 1)
        (->T 2)
        (->T 3)] '()))

(facts "either"
  ((either (*eq 1) (*eq 2)) '(2)) => (->Token 2 '())
  ((either (*eq 1) (*eq 2)) '(2 1)) => (->Token 2 '(1))
  ((either (*eq 1) (*eq 2)) '(3 2 1)) => nil
  ((either (*eq 1) (*eq 2)) '(1 2)) => (->Token 1 '(2))
  ((either (*eq 1)) '()) => nil
  ;; TODO can you possibly end with (->T 2)?
  ;;((*dump (*splice (either (*eq 1) (*splice (*group (*splice (*zero-or-more (*eq 2)))))))) '(2)) => (->T 2)
  )

(facts "but"
  ((*but 1) '(2)) => (->T 2)
  ((*but 1) '(2 3 4)) => (->Token 2 '(3 4))
  ((*but 1) '(1)) => nil
  ((*but 1) '()) => nil)

(facts "string"
  (*string '(\" \\ \" \")) => (->T "\"")
  (*string '(\" \")) => (->Token "" '())
  (*string (seq "\"a\"")) => (->Token "a" '())
  (*string (seq "\"abc\"")) => (->Token "abc" '())
  (*string (seq "\"abc\\\"d\"")) => (->Token "abc\"d" '())
  (*string (seq "123")) => nil
#_(parse *string (seq "\"123")) 
  )

(facts "number"
  (*number (seq "123")) => (t 123)
  (*number (seq "123 ")) => (->T 123  '(\space))
  (*number (seq " 123 ")) => nil ;;no whitespace in def, expected
  (*number (seq "\"123\"")) => nil)

(facts "reserved"
  (*null (seq "null"))=> (t "null")
  (*true (seq "true")) => (t "true")
  (future-fact "case insesitive"
    (*true (seq "TRUE")) => (t "true"))
  (*false (seq "false")) => (t "false"))

(facts "array"
  (*array (seq "[]")) => (t [])
  (*array (seq " [ ] ")) => (t [])
  (*array (seq "[1]")) => (->T [1])
  (*array (seq " [ 1 ] ")) => (t [1])
  (*array (seq " [ 1 , 1 ] ")) => (t [1 1])
  (parse *array (seq " [ 1  1 ] ")) => nil
  (*array (seq "[1,\"a\"]")) => (t [1 "a"])
  (*array (seq "[1,2,[1,2,[1,2,[1,2]]]]")) => (->T [1,2,[1,2,[1,2,[1,2]]]])
  (*array (seq "[[[[1],[[2]]]],[3]]")) => (t [[[[1] [[2]]]] [3]])
  (*array (seq "[[[[1],[[2]]]],[3,4]] xxxx")) => (t [[[[1] [[2]]]] [3 4]] '(\x \x \x \x))
  (parse *array (seq "[[[[1],[[2]]]],[3 4]]")) => nil
  (*array (seq "[10,11,\"ae\",[1,2,[3,4]]]")) => (t [10 11 "ae" [1 2 [3 4]]]))

(fact "value"
  ((*group *value *value) (seq "[1]1")) => (t [(->T [1]) (->T 1)])
  ((*repeat *value) (seq "[1]1")) => (t [(->T [1]) (->T 1)])
  (*value (seq " 1 ")) => (t 1)
  (*value (seq "1")) => (t 1)
  ((*repeat *value) (seq "1 1")) => (->T [(->T 1) (->T 1)])
  (*value (seq "\"1\"")) => (->T "1")
  ((*repeat *value) (seq "\"1\" 1")) => (->T [(->T "1") (->T 1)]))

(facts "whitespace"
  (*whitespace (seq "")) => (t nil)
  (*whitespace '(1 2)) => (->Token nil '(1 2))
  (*whitespace '(\newline)) => (t nil))

(facts "pair"
  ;; could use (defrecord Pair [key value] as well, but why?
  ;; key could be keyword...helping or not?
  (*pair (seq " \"k1\" : 123 ")) => (t {"k1" 123})
  (*pair (seq " \"k1\" : 123 ,")) =>(->Token {"k1" 123} '(\,))
  (*pair (seq "\"k1\" : [1]")) => (->T {"k1" [1]}))

(facts "object"
  (*object (seq "{}")) => (t {})
  (*object (seq " { \"x\" : 1 , \"f\" : 2 }"))  => (t {"x" 1 ,"f" 2})
  (*object (seq " { \"x\" : 1 , \"f\" : 2 } 123")) => (t {"x" 1 ,"f" 2} (seq " 123"))
  (*object (seq "{ \"a\":[1,2] , \"o\":{\"k\":1} }")) => (t {"a" [1 2],"o" {"k" 1}})
  )

(future-facts "benchmark"
  (doall (reduce (fn [a b]) (repeatedly 50 (fn [] (time (*string (seq "\"uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuhhhhhhhhhhhuuu\""))) nil)))))


(facts "fragment"
  (deffragment test1
  :thegroup ^:test-meta (*group (*eq 1) (*eq 2)))
  (test1 '(1 2)) => nil

#_(-> '(1 2) (deffragment test2 :thegroup (*group :part1 (*eq 1) :part2 (*eq 2))))
  )

(future-facts "metadata"
  
  (defn testxx [& ops]
    (let [t (->> ops
              (map (fn [o] (try {(print-str o) (meta o)})))
              (filter some?))]
      (println t)))

  (testxx :test ^:test ^{:doc "blaa"} (fn [_] (println "hello")) :eq ^:meta (*eq 1))

  ;;
  ((fn ^:auu auu [] (println (meta #'auu))))
  (defn ^:auu auu [] (println (meta #'auu)))
  )

;; ********************************************



(defn get-fns
  "returns map (symbol . var)"
  [nspace]
  (->>
    (ns-publics nspace)
    (filter #(-> % val var-get clojure.test/function?))))

(defn instrument-fn [nspace f ofn]
  (with-meta
    (fn [& args]
      (prof/p (keyword
                (name nspace)
                (name (key f)))
        (apply ofn args)))
    {:profiled "true"
     :original-fn ofn}))

(defn instrument [nspace]
  (let [fns (get-fns nspace)]
    (doseq [f fns]
      (alter-var-root (val f)
        (fn [ofn]
          (if (:profiled (meta ofn))
            (instrument-fn nspace f (:original-fn (meta ofn)))
            (instrument-fn nspace f ofn)
))))))

(defn go [f]
  (instrument 'json.grammar)

  (prof/profile :info :grammar
    (f))

  nil)

(future-facts "istrument"
  (instrument 'f.grammar.shared)

  (prof/profile :info :instrument
    ((gr/mk-pn :test
       (fn test [lines]
         (gr/mk-match "true" [])))
     ["a"])))



;; (def ss '(\" \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \u \"))

;; (go #(doall (take 90 (iterate (*eq \u) ((*eq \u) (rest ss))))))

#_(time
    (do
      (into [] (file-to-seq "/Users/gdanov/work/playground/regfunc/citm_catalog.json"))
      nil))




