(ns regfunc.macro-test
  (:require [clojure.test :refer :all]
            [regfunc.macro :refer :all]))

(defmethod assert-expr 'contains? [msg form]
  ;; Test if x is an instance of y.
  `(let [target# ~(nth form 1)
         val# ~(nth form 2)]
     (assert (map? val#))
     (assert (map? target#))
     (let [result# (every? true? (map (fn [[k# v#]] (= (get val# k#) v#)) target#))]
       (if result#
         (do-report {:type :pass, :message ~msg,
                     :expected target#, :actual val#})
         (do-report {:type :fail, :message ~msg,
                     :expected target#, :actual val#}))
       result#)))

(defmacro fm []
  `(defmacro cfm []
     (fn [] "hello world")))

(deftest macro-lifecycle
  (fm)
  (let [fun (eval `(cfm))
        fun-alt (cfm nil nil)]
    (is (= "hello world" (fun)))))

(deftest t-rule
  (rule test1 [a b] (str a " " b))
                                        ; (. #'test1 isMacro) => true
  (let [t* (fragment (test1 "hello"))]
    (is (fn? t*))
    (is (= (t* "world") "hello world")))

  (rule test1varg [& a b] (str (clojure.string/join ":" a) " " b))
  (let [t*va (fragment (test1varg "a" "b" "c"))]
    (is (fn? t*va))
    (is (= (t*va "d") "a:b:c d"))))

(deftest who-is-what
  (rule *test [a b] (println a))
  (def **test (fragment (*test 1))) 

  (testing "things to check are defined"
    (is (some? #'*test))
    (is (some? #'**test))
    (is (some? (-> `*test resolve var-get))))
  
  (testing "what fn and defn return"
    (is (fn? (fn blaa [])))
    (is (not (symbol? (fn bloo [])))) 
    (is (not (var?(fn bluu []))))
    (is (var? (defn blee [])))
    (is (not (fn? (defn blii []))))
    (is (fn? blii)))
  
  (testing "meta is on the rule macro var"
    ;; (meta <macro-symbol>) is impossible, always throws "Can't take value
    ;; of a macro:". see http://www.braveclojure.com/writing-macros/
    ;; for possible explanation

    ;; but this works
    ;; lein runs the tests in 'user, but *test gets defined in the right namespace
    (is (nil? (meta (var-get (ns-resolve 'regfunc.macro-test (symbol "*test")))))) 
    (is (nil? (meta (var-get #'*test)))) 
    
    ;;surprisingly this works directly
    (is (contains? {:regfunc.macro/rule true :macro true} (meta #'*test))))

  (testing "meta is on the rule parametrized function"
    (is (contains? {:regfunc.macro/rule.fn true} (meta **test)))
    (is (not (contains? {:regfunc.macro/rule.fn true} (meta #'**test)))))

  (testing "what is what"
    (is (not (symbol? **test)))
    (is (symbol? '**test))
      
    (is (not= #'**test (var-get #'**test)))
    (is (= **test (var-get (ns-resolve 'regfunc.macro-test (symbol "**test")))))
    
    (is (var? #'*test)) 
    (is (var? #'**test))

    (is (not (fn? #'*test)))
    (is (not (fn? #'**test)))

    (is (fn? **test))
    (is (fn? (var-get #'*test)))
    (is (fn? (var-get #'**test)))))

(deftest test-rule-macro?
  (rule bbb [a b] (identity b))
  (deffragment bbb-b (bbb nil)) ;; (let does not work as the symbol can't be resolved

  ;; this will *evaluate* to fn
  (is (false? (rule-macro? `(bbb 1))))
  (is (rule-macro? `bbb))

  ;; the fully-quallyfied quoting is essential!
  (is (rule-fn? `(bbb 1)))
  (is (rule-fn? `bbb-b))

  (is (rule-fn? bbb-b))

  ;; (ns-unmap 'regfunc.macro-test 'bbb-b)
  ;;(ns-unmap 'regfunc.macro-test 'bbb)
  )

(deftest test-fragment
  (rule *eq [a b] (= a b))
  (rule dump [a b] (identity `~ctx))
  (rule *group [& a b] (cons :group (map (fn [fun] (fun b)) a)))
  (deffragment *one (dump 1))

  (is (= '{:path [*one] :name *one} (*one nil)))
  ;; it's questionable if this makes sense at all. but at least it works without extra hacks 
  (is (= '{:path [*one] } ((fragment *one) nil))) 
  
  ;; (is (matching? sexp [path val] [path val]))
  (let [res (macroexpand-1 `(fragment (*eq 1)))]
    (is (= `*eq (first res)))
    (is (= 1 (last res))))
  (let [res (macroexpand-1 `(fragment (*group (*eq 1) (*eq 2))))]
    (is (= `*group (first res)))
    (is (= `(fragment  {:path ['*group]}(*eq 2)) (last res))))
  (let [res (macroexpand-1 `(fragment (*group (^some-name *group (*eq 1)))))]
    (is (rule-fn? (fragment (*group (*group (*eq 1))))))
    (is (= '(:group (:group true)) ((fragment (*group (*group (*eq 1))))1))))
  (is (= '(:group (:group {:path [*group *group dump]}))
        ((fragment (*group (*group (dump nil))))nil)))

  (is (= '(:group {:path [*group *one]}) ((fragment (*group *one)) nil))))

