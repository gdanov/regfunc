(ns regfunc.t-macro
  (:require [regfunc.macro :refer :all]
            [midje.sweet :refer :all]))

(facts "rule" 
  (rule test1 [a b] (str a " " b))
  (. #'test1 isMacro) => true
  (let [t* (test1 "hello")]
    t* => fn?
    (t* "world") => "hello world")

  (rule test1& [& a b] (str (clojure.string/join ":" a) " " b))
  (let [t*va (test1& "a" "b" "c")]
    t*va => fn?
    (t*va "d") => "a:b:c d")
  )

