(ns json.grammar
  (:require [regfunc.macro :refer :all]
            [annotate.types :refer :all]
            [clojure.zip :az zip]
            [annotate.fns :refer :all]
            [taoensso.timbre.profiling :as prof])
  (:import [java.io InputStreamReader ByteArrayInputStream FileInputStream Reader BufferedReader]
           [regfunc.macro Token])) 

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed) 

#_(defn take-while [pred col]
  (sequence
    (persistent!
      (reduce
        (fn [o v]
          (if (and pred (pred v))
            (conj! o v)
            (reduced o)))
        (transient [])
        col))))

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args more)))))

;; (token-fn) => #{nil, (->Token nil ..), (->Token some ..)}



(defn ->T
  ([val] (->Token val '()))
  ([val stream] (->Token val stream)))

(declare *object)
(declare *array)

;;----------------
(defn stream-to-seq [^Reader isr]
  (take-while
   (comp not nil?)
   (map #(if (= -1 %) nil (char %))
        (repeatedly #(.read isr)))))

(defn file-to-seq [fname]
  (stream-to-seq
    (BufferedReader.
      (InputStreamReader.
        (FileInputStream. ^String fname)))))

;;---------------

(defn mod-t [ t fu]
  (when (some? t) (->Token (fu (:value t)) (:stream t))))

;; TODO there is problem with arrays - () is used internally so they
;; can't be represented with lists. currently I pack them in vectors
;; using the fact that vectors are not ISeq


(defn **lin [v]
  (if (seq? v)
    (mapcat
      (fn [o] (if (seq? o) o (list o)))
      v)
    v))

(defn lin [fu]
  (fn [t] (mod-t (fu t) **lin)))

(def ^:dynamic fragment-parent '())

;; todo copy (some) metadata
(rule ^{:test-meta true
        :doc "simple literal equality"
        :arglist '([literal the-input-sequence])}
  *eq
  [c tseq]
  (when (= c (first tseq)) (->Token c (rest tseq))))

#_(rule group-ng [& ops inp]  )

#_(rule repeat-ng [op inp])

#_(deffragment eq-to-one
  ;; :eqlabel
  (*eq 1))

#_(deffragment fragii
  ;; :grpp nooo, the label is the fragment name!
  (group-ng ;; maybe-labeled-list
    :eqlabel (*eq 1)
    ;; ==> (*eq fqn label 1) ==> (let [my-label# :eqlabel my-fqn# (fragii eqlabel)] (fn **eq [inp] (.. middleware (my-label#) ... body ))
    :repeatlabel (repeat-ng (*eq 2))
    *array
    :the-obj object))

#_(deffragment testx
  :test ^:test ^{:doc "blaa"} (fn [_] (println "hello")) ;; illegal. must be wrapped in some macro
  (deffragment tistttt) ;; also illegal - you don't want to defn wile passing arguments
  (*eq 3) ;; the right way, rule or fragment symbol
  )
;(testx nil)

(rule *repeat [op tseq]
  
  (let [res (->> (iterate (comp op :stream) (op tseq))
              (take-while some?))]
    (when (seq res) (->Token res
                      (:stream (last res))))))

(defn *splice [fac-fun]
  (fn [strm]
    (some-> (fac-fun strm) (with-meta {:splice true}))))

(rule *zero-or-more [op tseq]
  ;"never returns nil as it's optional, however when there is no match the :value is null"
  (let [res ((*repeat op) tseq)]
    ; (throw (Exception. (print-str  res)))
    (if (nil? res)
      (->Token nil (if (nil? tseq) '() tseq))
       res)))

(rule *dump [o strm]
  (o strm))

(defn dump [lbl f]
  (fn [t]
    (let [r (f t)]
      (println "#dump#" lbl ":" t "=>" r)
      r)))

(rule *group [& ops tseq]
  [ops-count (count ops)]
  (let [res (->>
              (iterate
                (fn [[tok ops]]
                  (when (and
                          (seq ops)
                          (seq tok))
                    [((first ops) (:stream tok)) (rest ops)]))
                [((first ops) tseq) (rest ops)])
              (map first)
              (take-while some?)
              (take ops-count))]
    (when (= ops-count (count res))
      (->Token (filter some? res) (:stream (last res))))))

(rule either [& ops tseq]
  (reduce
    (fn [o v] (let [res (v tseq)]
                (when (some? res) (reduced res))))
    nil
    ops))

;;***********************

(rule *but [c tseq]
  ;;  "the opposite of *eq. always exactly one token is taken from the stream"
  [eq (*eq c)]
  (when (and (seq tseq) (not= c (first tseq))) (->Token (first tseq) (rest tseq))))

(rule *any [_ t]
  ;; consumes 1 token
  (->Token (first t) (rest t)))

(defn t
  ([v] (->Token v '()))
  ([v s] (->Token v s)))

(defmulti tostr (fn [v] (type v)))
(defmethod tostr Character [c] (str c))
(defmethod tostr clojure.lang.ISeq [s] (clojure.string/join s))
(defmethod tostr nil [_] "")

(defn mod-t-fn [f]
  (prof/fnp mod-t-fn-mk [fu]
    (prof/fnp mod-t-fn? [^Token t]
      (prof/p (keyword "mod-t-fn:" (str f))
        (mod-t (fu t) f)))))

(def *tostr (mod-t-fn (prof/fnp --tostr [v] (tostr v))))

(def *toint (mod-t-fn
              (fn [v] (when (some? v) (-> v ^String tostr ; num;Integer.
                                        ))))) ;; (num) is in in order to throw exception

(defn nil-fn [_] (identity nil))

(defn ignore [fu]
  (fn [t]
    (mod-t (fu t) nil-fn)))

(defn *pack [a] 
  (fn **pack [t]
    (mod-t (a t) vec)))

(def *esc-quot (prof/fnp *esc-quot [& a] (apply (*group (ignore (*eq \\)) (*eq \")) a)))

(rule *tostr [op strm]
  (let [res (op strm)
        chars (when res (map :value (:value res)))]
    (when (some? chars)
      (->T (String. (char-array (into-array chars))) (:stream res)))))

(def *string (*tostr
               (*group
                 (ignore
                   (*eq \"))
                 (*splice (*zero-or-more
                            (*splice (either
                                       *esc-quot
                                       (*but \")))))
                 (ignore (*eq \")))))

(def *whitespace
  (ignore (*zero-or-more (either (*eq \space) (*eq \return) (*eq \newline)))))

(rule *toInt [o strm]
  (let [res (o strm)]
    (when (some? res)
     (->T (BigInteger.
            ^String (:value res)) (:stream res)))))

(defmacro apply-macro
  "to be used for vararg macro like group and either to be applied on collection. vals bust be quoted. vals is collection"
  [ma vals]
  ; (println vals)
  ; (println (eval vals))
  `(~ma ~@(eval vals)))

(defmacro mmap
  "map for macros"
  [ma values]
  (let [res  (map (fn [v] `(~ma ~v)) (eval values))]
    `'(~@res)))

(def *number
  (*toInt (*tostr (*repeat (apply-macro either (mmap *eq '(\- \1 \2 \3 \4 \5 \6 \7 \8 \9 \0))))))
)

(def *true (*eq "true"))
(def *false (*eq "false"))
(def *null (*tostr (apply-macro *group (mmap *eq (seq "null")))))
(def *true (*tostr (apply-macro *group (mmap *eq (seq "true")))))
(def *false (*tostr (apply-macro *group (mmap *eq (seq "false")))))

(def *value (fn [strm]
              (some-> strm
                ((*group
                   *whitespace
                   (*splice
                     (either
                       *string
                       *number
                       #'*object
                       #'*array
                       *true
                       *false
                       *null
                       ))
                   *whitespace))
                ((fn [to] (let [v (some-> to :value first)
                                s (some-> to :stream)]
                            (some-> v (assoc :stream s))))))))

(def closing-bracket (ignore (*group *whitespace (*eq \]) *whitespace)))


(def *array (fn [strm]
              (let [res ((*group
                           (ignore (*group *whitespace (*eq \[) *whitespace))
                           (either
                             (*group
                               #'*value
                               (either
                                 closing-bracket
                                 (*group
                                   (*repeat
                                     (*group
                                       (ignore (*eq \,))
                                       ;; TODO
                                       #'*value))
                                   closing-bracket)))
                             closing-bracket))
                         strm)]
                (some->> (:value res)
                  (map :value)
                  vec
                  (#(->T % (:stream res)))))))

(def *pair (fn [strm]
             (-> strm
               ((*group *whitespace *string *whitespace (ignore (*eq \:)) *whitespace #'*value))
               ((fn [r]
                  (when r (->T {(:value (first (:value r))) (:value (last (:value r)))} (:stream r))))))))

(def *object (fn [strm]
               (let [res (-> strm
                           ((*group
                              *whitespace
                              (ignore (*eq \{))
                              (*zero-or-more
                                (*group
                                  #'*pair 
                                  (*zero-or-more
                                    (*group
                                      (ignore (*eq \,))
                                      #'*pair))))
                              *whitespace
                              (ignore (*eq \}))
                              )))]
                 (when res (->T (reduce merge  {} (map :value (:value res))) (:stream res))))))

(def *json (either *array *object))

(defn parse [r strm]
  (binding [last-fail-path nil]
    (let [res (r strm)]
      (when (nil? res) (println "!nomatch!" last-fail-path))
      res)))

#_(*group
  *whitespace
  ^:splice (*zero-or-more ))
