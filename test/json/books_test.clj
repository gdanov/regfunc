(ns json.books-test 
  (:require  [regfunc.core :refer :all]
             [regfunc.macro :refer :all]
             [json.grammar :refer [*json]]
             [clojure.test :refer :all]
             [clojure.zip :as z]
             [clojure.walk :refer [keywordize-keys]])
  (:import [java.net URI]))

(defrecord Book [name authors])
(defrecord Author [name urn])
(defrecord AuthorRef [urn])

(def sample (update-in (*json (file-to-seq "test/json/books.json"))
              [:value] keywordize-keys))

;;transformation *without validation*
(def manual-transform
  {:books (map (fn [book] (->Book (:name book) (:authors book)))
            (:books (:value sample)))
   :authors (map 
              (fn [author] (map->Author {:name (:name author) :urn (:id author)}))
              (:authors (:value sample)))})

(rule *string [_ strm]
  (when (string? strm) (->T strm)))

(deftest t-str
  (is (= (->T "test") ((fragment (*string nil)) "test"))))

(rule *array [element-rule strm]
  (assert (vector? strm) (print-str strm))
  (let [res (map element-rule strm)]
    ;; TODO check that stream is empty
    (when (every? some? res)
      (->T (into [] (map :value res))))))

(deftest t-array
  (is (= (->T [1 2 3]) ((fragment (*array ->T)) [1 2 3]))))

;; operates the whole map
(rule *map-entry [& args strm] [[the-key field-rule] args]
  (assert (map? strm))
  (when (contains? strm the-key)
    (let [res (field-rule (get strm the-key))]
      ;; TODO check if the me value was completely consumed
      (when (some? res) (->T {the-key (:value res)} (dissoc strm the-key))))))

(deftest t-map-entry
  (is (= (->T {:name "the-name"} {})
        ((fragment (*map-entry :name (*string nil))) {:name "the-name"})))
  (is (= (->T {:name "the-name"} {:test "test"})
        ((fragment (*map-entry :name (*string nil))) {:name "the-name" :test "test"}))))

;; all entries must be present
;; does not copy unknown keys
(rule *map [& entries strm]
  (assert (map? strm) (print-str strm))
  (let [res (reduce
               (fn [res entry]
                 (let [r (entry strm)]
                   (assert (or (nil? r) (map? r)))
                   (if (nil? r) (reduced nil) (merge res (:value r)))))
               {}
               (seq entries))]
    (when res (->T res))))

(deffragment a-string (*string nil))

(deftest t-map
  (is (= (->T {"hello" "world"}) ((fragment (*map (*map-entry "hello" a-string))) {"hello" "world"}))))

;; so, what's the point of the whole thing? effectively it's all about verification. There is no transformation step (yet)

(deffragment urn ;*ctor (fn [s] (URI. s))
  (*tostr (*group (*repeat (*but \:)) (*eq \:) (*repeat (*but \:)))))
(deffragment a-name (*map-entry :name a-string))
(deffragment a-id (*map-entry :id urn))
(deffragment author
  (*ctor
    (comp
      map->Author
      ;; basic transformation
      (fn [o] (-> o (assoc :urn (:id o)) (dissoc :id))))
    (*map a-name a-id)))
(deffragment authors (*map-entry :authors (*array author)))

;; big problem is that the rule body is a fn so it can't do (*either me1 me2 ...). It sees already the parametrized fn and I can't (apply *either ...).
;; I must fix the macro expansion...
(deffragment book (*ctor map->Book
                    (*map a-name
                      (*map-entry :authors (*array urn)))))
(deffragment books (*map-entry :books (*array book)))

(deffragment books-obj (*map books authors))

(deftest t-book
  (let [r {:name "b1" :authors ["au:123"]}]
    (is (= (map->Book r) (:value (book r))))))

(deftest t-book-obj
  (is (= manual-transform (:value (books-obj (:value sample))))))

(deftest t-author
  (is (= (->T "id:123") (urn "id:123")))
  (is (= (->Author "author"  "au:111") (:value (author {:name "author" :id "au:111"}))))
  (is (= {:authors [(->Author "author one" "au:123")]} (:value (authors {:authors [{:name "author one",:id "au:123"}]})))))
