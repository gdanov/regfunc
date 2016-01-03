(ns json.grammar-test
  (:require [json.grammar :as gr :refer :all :exclude [one-or-more]]
            [clojure.test :refer :all]
            [regfunc.macro :refer :all]
            [regfunc.core :refer :all])
  (:import [java.io InputStreamReader ByteArrayInputStream FileInputStream BufferedReader]))

(defmacro future-fact [& _])
(defmacro future-facts [& _]) 

(deffragment *one (*eq 1))

(defmacro equals [left right]
  `(is (= ~left ~right)))

(deftest test-eq
  (equals (*one '(1)) (->Token 1 '()))
  (equals (*one '(1 2)) (->Token 1 '(2)))
  (is (nil? (*one '(2))))
  (is (nil?  (*one '())))
  (is (nil?  (*one nil)))
  (is (nil?  (*one (->Token 1 '())))))

(deftest t-eqStr
  (equals nil ((fragment (*eqStr "hello")) '()))
  (equals (->T "hello") ((fragment (*eqStr "hello")) (seq "hello")))
  (equals (->T "hello" (seq "xx")) ((fragment (*eqStr "hello")) (seq "helloxx")))
  )

(deffragment *repeat-one (*repeat (*eq 1)))
(deffragment *group-1-2 (*group (*eq 1) (*eq 2)))

(deftest repeat
  (equals (*repeat-one '(1 1)) (->T [(->T 1) (->T 1)]))
  (equals (*repeat-one '(1 1 2)) (->Token [(->T 1) (->T 1)] '(2)))
  (is (nil? (*repeat-one '(2 1))))
  (is (nil? (*repeat-one '()))) 
  (is (nil? (*repeat-one nil))) 
  ;((*repeat (*group (*eq 1)))'(1 1)) => ^:repeat (->T [(->T 1) (->T 1 )])
 
  ;; without the :consumed add-on on the Token enters into endless loop !!!
  (equals nil ((fragment (*repeat *whitespace)) (seq "a")))
  ;; (*repeat (*zero-or-more whatever))
  )

(deffragment *ignore-one (*ignore (*eq 1)))
(rule rule-for-ignore [_ _]
  (assoc (->Token nil '()) :something "something"))

(deftest t-ignore
  (testing "extra keys on the resulting Token are transmitted"
    (equals "something" (:something ( (fragment (*ignore (rule-for-ignore nil))) '()))))
  (equals (->T nil) (*ignore-one '(1)))
  (equals nil (*ignore-one '(2))))

(deftest t-splice
  (equals (->T [^:eq (->T 1) ^:eq (->T 1)])
    ((fragment (*repeat (*splice (*group (*eq 1))))) '(1 1)))
  ;; no effect without outer rule
  (equals (->T [(->T 1) (->T 2)])
    ((fragment (*splice *group-1-2)) '(1 2)))
  ;; now with the outer rule, the inner group is "transparent"
  (equals (->T [(->T 1) (->T 2)])
    ((fragment (*group (*splice *group-1-2))) '(1 2)))
  (equals (->T [(->T 2)])
    ((fragment (*group (*splice (*zero-or-more (*splice (*eq 2)))))) '(2)))) 

(deffragment *zero-or-more-one (*zero-or-more (*eq 1)))

(deftest zero-or-more
  (equals (->T [(->T 1) (->T 1)] '()) (*zero-or-more-one '(1 1)))
  (equals (->T [(->T 1) (->T 1)] '(2)) (*zero-or-more-one '(1 1 2)))
  (equals (assoc (->T nil '(1 1)) :consumed false) ((fragment (*zero-or-more (*eq 2))) '(1 1))) 
  (equals (assoc (->T nil) :consumed false) (*zero-or-more-one '()))
  (equals (assoc (->T nil) :consumed false) (*zero-or-more-one nil))

  (equals ((fragment (*zero-or-more (*group (*eq 1)))) '(1)) (->T [(->T 1)]))

;  ((*zero-or-more (*splice (*group (*eq 1)))) '(1)) => (->T [(->T 1)])
  )

(deftest group
  (equals (*group-1-2 '(1 2)) (->T [(->T 1 ) (->T 2)]))
  (equals (*group-1-2 '(1 2 3)) (->T [(->T 1) (->T 2 )] '(3)))
  (equals (*group-1-2 '(1 1 2)) nil)
  (equals (*group-1-2 '(1)) nil)
  (equals ((fragment (*group (*eq 0) *group-1-2 (*eq 3))) '(0 1 2 3)) 
    (->T [(->T 0)
          (->T 1)
          (->T 2)
          (->T 3)] '()))
  (equals ((fragment (*group (*eq 0) (*splice *group-1-2) (*eq 3))) '(0 1 2 3))
    (->T [(->T 0)
          (->T 1)
          (->T 2)
          (->T 3)] '())))

(deffragment *either-1-2 (*either (*eq 1) (*eq 2)))

(deftest t-either
  (equals (*either-1-2 '(2)) (->Token 2 '()))
  (equals (*either-1-2 '(2 1)) (->Token 2 '(1)))
  (equals (*either-1-2 '(3 2 1)) nil)
  (equals (*either-1-2 '(1 2)) (->Token 1 '(2)))
  (equals ((fragment (*either (*eq 1))) '()) nil)
  ;; TODO can you possibly end with (->T 2)?
  ;;((*dump (*splice (*either (*eq 1) (*splice (*group (*splice (*zero-or-more (*eq 2)))))))) '(2)) => (->T 2)
  )

(deffragment *but-1 (*but 1))

(deftest t-but
  (equals (*but-1 '(2)) (->T 2))
  (equals (*but-1 '(2 3 4)) (->Token 2 '(3 4)))
  (equals (*but-1 '(1)) nil)
  (equals (*but-1 '()) nil))

(deftest t-pred
  (equals (->T 1 '()) ((fragment (*pred(fn [o] (#{1 2} o)))) '(1)))
  (equals (->T 1 '()) ((fragment (*pred #{1 2})) '(1))))

(deftest t-string
  (equals (->T "\"") (*string '(\" \\ \" \"))) 
  (equals (->Token "" '()) (*string '(\" \")))
  (equals (->Token "a" '()) (*string (seq "\"a\"")))
  (equals (->Token "abc" '()) (*string (seq "\"abc\"")))
  (equals (->Token "abc\"d" '()) (*string (seq "\"abc\\\"d\"")))
  (equals nil (*string (seq "123")))
#_(parse *string (seq "\"123")) 
  )

(deftest t-number
  (equals (t 123) (*number (seq "123")))
  (equals (->T 123  '(\space)) (*number (seq "123 ")))
  (equals nil (*number (seq " 123 "))) ;;no whitespace in def, expected
  (equals nil (*number (seq "\"123\""))))

(deftest t-reserved
  (equals (t "null") (*null (seq "null")))
  (equals (t "true") (*true (seq "true")))
  (equals (t "TRUE") (*true (seq "TRUE")))
  (equals (t "false") (*false (seq "false"))))

(deftest t-array
  (equals (t []) (*array (seq "[]")))
  (equals (t []) (*array (seq " [ ] ")))
  (equals (->T [1]) (*array (seq "[1]")))
  (equals (t [1]) (*array (seq " [ 1 ] ")))
  (equals (t [1 1]) (*array (seq " [ 1 , 1 ] ")))
  (equals nil (*array (seq " [ 1  1 ] ")))
  (equals (t [1 "a"]) (*array (seq "[1,\"a\"]")))
  (equals (->T [1,2,[1,2,[1,2,[1,2]]]]) (*array (seq "[1,2,[1,2,[1,2,[1,2]]]]")))
  (equals (t [[[[1] [[2]]]] [3]]) (*array (seq "[[[[1],[[2]]]],[3]]")))
  (equals (t [[[[1] [[2]]]] [3 4]] '(\x \x \x \x)) (*array (seq "[[[[1],[[2]]]],[3,4]] xxxx")))
  (equals nil (parse *array (seq "[[[[1],[[2]]]],[3 4]]")))
  (equals (t [10 11 "ae" [1 2 [3 4]]]) (*array (seq "[10,11,\"ae\",[1,2,[3,4]]]"))))

(deftest t-value
  (equals (t [(->T [1]) (->T 1)]) ((fragment (*group *value *value)) (seq "[1]1")))
  (equals (t [(->T [1]) (->T 1)]) ((fragment (*repeat *value)) (seq "[1]1")))
  (equals (t 1) (*value (seq " 1 ")))
  (equals (t 1) (*value (seq "1")))
  (equals (->T [(->T 1) (->T 1)]) ((fragment (*repeat *value)) (seq "1 1")))
  (equals (->T "1") (*value (seq "\"1\"")))
  (equals (->T [(->T "1") (->T 1)]) ((fragment (*repeat *value)) (seq "\"1\" 1")))
  )

(deftest t-whitespace
  (equals (assoc (t nil) :consumed false) (*whitespace (seq "")))
  (equals (assoc (->Token nil '(1 2)) :consumed false) (*whitespace '(1 2)))
  (equals (t nil) (*whitespace '(\newline)))
  )

(deftest t-pair
  ;; could use (defrecord Pair [key value] as well, but why?
  ;; key could be keyword...helping or not?
  (equals (t {"k1" 123}) (*pair (seq " \"k1\" : 123 ")))
  (equals (->Token {"k1" 123} '(\,)) (*pair (seq " \"k1\" : 123 ,")))
  (equals (->T {"k1" [1]}) (*pair (seq "\"k1\" : [1]"))))
 
(deftest t-object
  (equals (t {}) (*object (seq "{}")))
  (equals (t {"x" 1 ,"f" 2}) (*object (seq " { \"x\" : 1 , \"f\" : 2 }")))
  (equals (t {"x" 1 ,"f" 2} (seq " 123")) (*object (seq " { \"x\" : 1 , \"f\" : 2 } 123")))
  (equals (t {"a" [1 2],"o" {"k" 1}}) (*object (seq "{ \"a\":[1,2] , \"o\":{\"k\":1} }")))
  )

(future-facts "benchmark"
  (doall (reduce (fn [a b]) (repeatedly 50 (fn [] (time (*string (seq "\"uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuhhhhhhhhhhhuuu\""))) nil)))))


(future-facts "fragment"
  (deffragment test1
  :thegroup ^:test-meta *group-1-2)
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

#_(time
    (do
      (into [] (file-to-seq "/Users/gdanov/work/playground/regfunc/citm_catalog.json"))
      nil))

#_ (time
     (do
       (spit "out.txt"
         (print-str (*object (file-to-seq "/Users/gdanov/work/playground/regfunc/citm_catalog.json"))))
       nil))



