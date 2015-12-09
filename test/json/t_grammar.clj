(ns json.t-grammar
  (:require [json.grammar :as gr :refer :all :exclude [one-or-more parse]]
            [midje.sweet :refer :all :exclude [one-of anything]]
            [taoensso.timbre.profiling :as prof])
  (:import [java.io InputStreamReader ByteArrayInputStream FileInputStream BufferedReader]))

(facts "get-token properly separates head from tail"
  ;; this is intentionally so that the matchers can just say
  ;; (get-token arg) regardless what is the argument type
       (get-token (seq "abc")) => (->Token \a '(\b \c))
       (get-token (->Token \a '(\b \c))) => (->Token \b '(\c))
       (get-token '()) => nil)

(facts "token-seq"
  (token-seq '(1 2 3)) => [(->Token 1 '(2 3)) (->Token 2 '(3)) (->Token 3 '())]
  (token-seq (->Token 1 '(2 3))) => [(->Token 2 '(3)) (->Token 3 '())])

(facts "match"
       ((*eq 1) '(1)) => (->Token 1 '())
       ((*eq 1) '(1 2)) => (->Token 1 '(2))
       ((*eq 1) '(2)) => nil
       ((*eq 1) '()) => nil
       ((*eq 1) nil) => nil
       ((*eq 1) (->Token 5 '())) => nil)

(future-facts "decorate")

(facts "repeat"
       ((*repeat (*= 1)) '(1 1)) => (->Token '(1 1) '())
       ((*repeat (*= 1)) '(1 1 2)) => (->Token '(1 1) '(2))
       ((*repeat (*= 1)) '(2 1)) => nil
       ((*repeat (*= 1)) '()) => nil
       ((*repeat (*= 1)) nil) => nil
       ((*repeat (*group (*= 1)))'(1 1 1)) => (t '(1 1 1)))

(facts "zero-or-more"
       ((*zero-or-more (*= 1)) '(1 1)) => (->Token '(1 1) '())
       ((*zero-or-more (*= 1)) '(1 1 2)) => (->Token '(1 1) '(2))
       ((*zero-or-more (*= 2)) '(1 1)) => (->Token nil '(1 1)) 
       ((*zero-or-more (*= 1)) '()) => (->Token nil '())
       ((*zero-or-more (*= 1)) nil) => (t nil))

(facts "group"
       ((*group (*= 1) (*= 2)) '(1 2)) => (->Token '(1 2) '())
       ((*group (*= 1) (*= 2)) '(1 2 3)) => (->Token '(1 2) '(3))
       ((*group (*= 1) (*= 2)) '(1 1 2)) => nil
       ((*group (*= 1) (*= 2) (*= 3)) '(1 2 3)) => (->Token '(1 2 3) '())
       ((*group (*= 1) (*= 2)) '(1)) => nil
       ((*group (*= 1) (*= 2)) '(1 2 3)) => (->Token '(1 2) '(3))
       ((*group (*= 1) (*= 2)) (->Token 5 '())) => nil
       ((*group (*= 0) (*group (*= 1) (*= 2)) (*= 3)) '(0 1 2 3)) => (->Token '(0 1 2 3) '())
       ((*group (*= 0) (*group (*= 1) (*= 2) (*group (*= 22))) (*= 3)) '(0 1 2 22 3)) => (->Token '(0 1 2 22 3) '())
       ((*group (*repeat (*= 1))) '(1 1)) => (t '(1 1))
       )

(facts "either"
  ((either (*= 1) (*= 2)) '(2)) => (->Token 2 '())
  ((either (*= 1) (*= 2)) '(2 1)) => (->Token 2 '(1))
  ((either (*= 1) (*= 2)) '(3 2 1)) => nil
  ((either (*= 1) (*= 2)) '(1 2)) => (->Token 1 '(2))
  ((either (*= 1)) '()) => nil)

(facts "but"
  ((*but 1) '(2)) => (->Token 2 '())
  ((*but 1) '(1)) => nil
  ((*but 1) '()) => nil)

(facts "string"
  (*string (seq "\"\"")) => (->Token "" '())
  (*string (seq "\"a\"")) => (->Token "a" '())
  (*string (seq "\"abc\"")) => (->Token "abc" '())
  (*string (seq "\"abc\\\"d\"")) => (->Token "abc\"d" '()))

(facts "number"
  (*number (seq "123")) => (t 123)
  (*number (seq " 123 ")) => nil ;;no whitespace in def, expected
  )

(facts "reserved"
  (*null (seq "null"))=> (t "null")
  (*true (seq "true"))=> (t "true")
  (*true (seq "TRUE")) => (t "true")
  (*false (seq "false"))=> (t "false"))

(facts "array"
  (*array (seq "[]")) => (t [])
  (*array (seq " [ ] ")) => (t [])
  (*array (seq "[1]")) => (t [1])
  (*array (seq " [ 1 ] ")) => (t [1])
  (*array (seq " [ 1 , 1 ] ")) => (t [1 1])
  (*array (seq "[1,\"a\"]"))=> (t [1 "a"])
  (*array (seq "[[[[1],[[2]]]],[3]]")) => (t [[[[1] [[2]]]] [3]])
  (*array (seq "[10,11,\"ae\",[1,2,[3,4]]]")) => (t [10 11 "ae" [1 2 [3 4]]]))

(fact "value"
  ((*group *value *value) (seq "[1]1")) => (t '([1] 1))
  ((*repeat *value) (seq "[1]1")) => (t '([1] 1))
  (*value (seq " 1 ")) => (t '(1));;that's ok, due to the *group. can't avoid that?
  )

(facts "whitespace"
  (*whitespace (t 1)) => (t nil)
  (*whitespace '(1 2)) => (->Token nil '(1 2))
  (*whitespace (->Token nil '(1))) => (->Token nil (->Token nil '(1))))

(facts "pair"
  (*pair (seq " \"k1\" : 123 ")) => (t ["k1" 123])
  (*pair (seq " \"k1\" : 123 ,")) =>(->Token ["k1" 123] '(\,)))

(facts "object"
  (*object (seq "{}")) => (t '())
  (*object (seq " { \"x\" : 1 , \"f\" : 2 }")) => nil
  (*object (seq "{ \"a\":[1,2] , \"o\":{\"k\":1} }")) => nil
  )

(future-facts "benchmark"
  (doall (reduce (fn [a b]) (repeatedly 50 (fn [] (time (*string (seq "\"uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuhhhhhhhhhhhuuu\""))) nil)))))


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
