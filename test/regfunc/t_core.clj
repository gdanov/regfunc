(ns regfunc.t-core
  (:require [regfunc.core :as gr :refer :all :exclude [one-or-more parse]]
            [midje.sweet :refer :all :exclude [one-of anything]]))

(defn create-tag [v]
  v)

(defn tag* [t]
  (mk-pn
    (fn [lines]
      (let [r (= t (first lines))]
        (if (= t (first lines))
          (mk-match (create-tag t) (rest lines))
          (assoc
            (mk-nomatch lines)
            :location (str "tag[" t "]")))))))

(defn att*
  "stub to inject tag with attributes without the need to provide that in the input"
  [tag attrs]
  (mk-pn
    (fn [lines]
      (mk-match {:name tag :attribs attrs} lines))))
  
(facts "mk-pn"
  ;; mystery ((mk-pn (fn [lines] nil)) ["aeuaeu"]) => (throws Exception)
  ((mk-pn (fn [lines] [])) ["aeuaeu"]) => []
  ;; mystery ((mk-pn (fn [lines] [123])) ["aeuaeu"]) => (throws Exception)
  )

(facts "tag"
  (fact "walks like a fn & quacks like a fn BUT is not a fn"
    (tag* "xx") => #(instance? clojure.lang.IFn %)
    (tag* "x") =not=> fn?
    ((tag* "x") ["x"]) => matches?
    (apply (tag* "xx") ["xx"]) => matches?
    )
  
  (fact "nomatch"
    (let [t ((tag* "xx") ["tadaaa"])]
      t =not=> matches?
      (remaining-lines t) => ["tadaaa"]
      (result t) => nil))

  (fact "match"
    (let [t ((tag* "AA") ["AA"
                          "line2"])]
      t => matches?
      (result t) => (create-tag "AA")
      (remaining-lines t) => ["line2"]
      ))

  (fact "EOF does not cause NPE"
    ((tag* "A") []) =not=> matches? )
  )

(facts "group"
  (facts "basics"
    (fact "match"
      (gr/parse (gr/group (tag* "A") (tag* "B")) ["A" "B"])
      => (result-is [(create-tag "A") (create-tag "B")]))

    (fact "no match"
      (gr/parse (gr/group (tag* "A") (tag* "B")) ["A" "X"])
      =not=> matches?)

    (fact "single value"
      ((group (tag* :a)) [:a]) => (result-is :a)))
  
  (facts "collections are properly produced"
    (fact "TODO there -are- were some NPEs when there is nomatch!"
      ((gr/group (tag* "A")) ["B"]) =not=> matches?)
    
    (fact "group of groups produces flat array"
      (gr/parse
        (group
          (group (tag* "A") (tag* "B"))
          (group (tag* "C") (tag* "D")))
        ["A" "B" "C" "D"])
      => (result-is (map create-tag ["A" "B" "C" "D"])))
    
    (fact "group of repeats produces flat array"
      (gr/parse
        (group
          (gr/one-or-more (tag* "A"))
          (tag* "D"))
        ["A" "A" "A" "D"])
      => (result-is (map create-tag ["A" "A" "A" "D"]))))

  (fact "group (ignore"

    ((group (tag* "A") ($ignore (tag* "B"))) ["A" "B"])
    => (every-checker
         matches?
         (result-is (create-tag "A"))
         #(empty? (remaining-lines %))))

  (future-facts "partial matches"

    ((group (tag* ":a") (tag* ":b") (tag* ":c")) [":a" ":b" ":d"]) => nil
    ((group (tag* :a) (group (tag* :a) (group (tag* :a) (group (tag* :a) (tag* :b)))))
     [:a :a :a :a :x])=> nil 
    )
  )

(facts "ignore"

  (($ignore (tag* "A")) ["A" "B"]) => (every-checker
                                        matches?
                                        result-is-nil
                                        #(= 1 (count (remaining-lines %)))))

(facts "repeat"
  (facts "basics"
    (let [gram (gr/one-or-more (tag* "A"))]
      (gr/parse gram ["A" "A" "A"])
      => (result-is (repeat 3 (create-tag "A")))

      (gr/parse gram ["A" "A" "A" "noise"])
      => (every-checker
           (result-is (repeat 3 (create-tag "A")))
           #(-> (remaining-lines %)
              (= ["noise"])))
      
      (gr/parse gram ["garbage"]) => not-matches?))
  
  ;; nothing to test, see the group facts (group (repeat ...))
  (fact "group (repeat)")

  (future-fact "repeat maybe - ends??")

  (fact "(child (repeat"
    (->> ["A" "A" "A" "noise"]
      (($node ($child (gr/one-or-more (tag* "A")))))
      result
      :child) =>
    (contains (repeat 3 (create-tag "A"))))

  (fact "repeat (group"
    (result ((gr/one-or-more (group (tag* "A"))) ["A"])) => ["A"]
    (result ((gr/one-or-more (one-of (group (tag* "A")))) ["A"])) => ["A"])
  )

(facts "node"
  (fact "is composable"
    ($node (mk-pn (fn [x])) (mk-pn (fn [x]))) => #(instance? clojure.lang.IFn %))
  
  (fact "has the right structure"
    (let [gr ($node
               ($root (tag* "AA"))
               ($child (tag* "BB"))
               ($child (tag* "IGNORED"))
               (tag* "IGNORED"))
          lines ["AA"
                 "BB"
                 "IGNORED"
                 ;; first tag is consumed, but second is igrored! that's because it is not wrapped in some $xx fn
                 "IGNORED"
                 "XX"]
          res (gr lines)]
      
      res => matches?
      (remaining-lines res) => [(last lines)]
      (result res) => {:child [(create-tag "BB") (create-tag "IGNORED")]
                       :root (create-tag "AA")}
      ))

  (facts "$val"
    (($node
       ($root (tag* "1"))
       ($val :two (tag* "2")))
     ["1" "2"])
    => (result-is {:root "1" :two "2"}))

  (facts "$ignore"
    (($node
       ($root (tag* "1"))
       ($ignore
         (gr/one-or-more (tag* "2"))))
     ["1" "2"])
    => (result-is {:root "1"}))

  (facts "$child"
    (($node
       ($child (tag* "a"))
       ($child (tag* "b")))
     ["a" "b"])
    => (result-is {:child ["a" "b"]}))
   
  (facts "$merge"
    (($node
       ($val :test (tag* "a"))
       ($merge (att* "ATTR" [1 2 3])))
     ["a" "b"])
    => (result-is {:test "a" :name "ATTR" :attribs [1 2 3]}))
  )

(facts "maybe"
  ((maybe (tag* "A")) ["A"]) => (every-checker
                                       matches?
                                       (result-is (create-tag "A"))
                                       #(-> % remaining-lines empty?))
  ((maybe (tag* "A")) ["B"]) => (every-checker
                                       matches?
                                       result-is-nil
                                       #(-> % remaining-lines seq))
  )

(facts "one-of"
  (let [lines ["B"]
        gr (one-of (tag* "A") (tag* "B"))]

    (gr ["B"]) => matches?
    ;; result????
    (gr ["Z"]) =not=> matches?
    ))

(facts "zero-or-more"

  (fact "order"
    ((zero-or-more
       (one-of
         (tag* :a)
         (tag* :b))) [:b :b :a]) => (result-is [:b :b :a])

         ((zero-or-more (anything :capture? true)) [:1 :2 :3]) => (result-is [:1 :2 :3]))
  
  (let [gr (zero-or-more (tag* "A"))]

    (gr ["A" "A" "A" "B"])
    => (every-checker
         matches?
         #(= (-> % remaining-lines) ["B"])
         (result-is (repeat 3 (create-tag "A"))))
    
    (gr ["BBB"]) => (every-checker
                      matches?
                      #(= (remaining-lines %) ["BBB"])
                      result-is-nil)
    (gr ["BBB" "A"]) => (every-checker
                          matches?
                          #(= (remaining-lines %) ["BBB" "A"])
                          ;; BUT!!!
                          result-is-nil)
    ))

(facts "anything"
  (let [tag-a (tag* :a)]
    
    ((group (anything) tag-a) [:b :a]) => (result-is :a)

    ((one-of (anything) tag-a) [:a]) => (result-is nil)
    ((one-of tag-a (anything)) [:a]) => (result-is :a)

    ((one-of tag-a (anything)) [:b]) => (result-is nil)

    ((group (zero-or-more (anything)) tag-a) [:b :b :a]) =not=> matches? ; anything is greedy and consumes all 3 tokens

    ((group (zero-or-more (anything :stop tag-a)) tag-a) [:b :b :a]) => (result-is :a)
    ((zero-or-more (one-of tag-a (anything))) [:b :b :a]) => (result-is '(:a))

    ((group (zero-or-more (anything :stop tag-a)) tag-a (tag* :x)) [:b :b :a :x]) => (result-is '(:a :x))

    ((one-of
       (group tag-a (anything :capture? true :stop (tag* :c)))
       (tag* :x)) [:a :b :c :c]) => (result-is [:a :b])
    )
  )


