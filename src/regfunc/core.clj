(ns regfunc.core
  (:require [regfunc.macro :refer :all]
            [annotate.types :refer :all]
            [clojure.zip :as zip]
            [annotate.fns :refer :all]
            [taoensso.timbre.profiling :as prof])
  (:import [java.io InputStreamReader ByteArrayInputStream FileInputStream Reader BufferedReader FileReader]
           [regfunc.macro Token]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed) 

;;----------------
(defn stream-to-seq [^Reader isr]
  (take-while
   some?
   (map #(if (= -1 %) nil (char %))
        (repeatedly #(.read isr)))))

(defn file-to-seq [fname]
  (stream-to-seq
     ; (FileInputStream. ^String fname)
    (BufferedReader.
      (FileReader. ^String fname))))
;;---------------

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args more)))))

(defn ->T
  ([val] (->Token val '()))
  ([val stream] (->Token val stream)))

(rule ^{:test-meta true
        :doc "simple literal equality"
        :arglist '([literal the-input-sequence])}
  *eq
  [c tseq]
  (when (= c (first tseq)) (->Token c (rest tseq))))

(rule *repeat [op tseq]
  (let [res (->> (iterate (comp op :stream) (op tseq))
              (take-while #(and (not (false? (:consumed %))) (some? %))))]
    (when (seq res) (->Token res
                      (:stream (last res))))))

(rule *splice [fac-fun strm]
  (some-> (fac-fun strm) (with-meta {:splice true})))

(rule *zero-or-more [op tseq]
  ;"never returns nil as it's optional, however when there is no match the :value is null"
  (let [res ((*repeat nil op) tseq)]
    (if (nil? res)
      (-> (->Token nil (if (nil? tseq) '() tseq)) (assoc :consumed false))
       res)))

(rule *pred [p strm]
  (when (p (first strm)) (->Token (first strm) (rest strm))))

(rule dump [& args strm] [[lbl f] args]
  (let [r (f strm)]
    (println "#dump#" lbl ":"  "=>" r)
    r))

(rule *group [& ops tseq]
  [ops-count (count ops)
   xf (comp (map first) (take-while some?) (take ops-count))]
  (let [res (->>
              (iterate
                (fn [[tok ops]]
                  (when (and
                          (seq ops)
                          (seq tok))
                    [((first ops) (:stream tok)) (rest ops)]))
                [((first ops) tseq) (rest ops)])
              (sequence xf))]
    (when (= ops-count (count res))
      (->Token res ;(filter some? res)
        (:stream (last res))))))

(rule *either [& ops tseq]
  (reduce
    (fn [o v] (let [res (v tseq)]
                (when (some? res) (reduced res))))
    nil
    ops))

;;***********************

;; name is bit misleading, as random transformation fn can be used.
(rule *ctor [& args strm]
  [[ctor op] args]
  (let [r (op strm)]
   (when (some? r) (->T (ctor (:value r)) (:stream r)))))

;; TODO 
(rule *not [op strm])

(rule *but [c tseq]
  ;;  "the opposite of *eq. always exactly one token is taken from the stream"
  (when (and (seq tseq) (not= c (first tseq))) (->Token (first tseq) (rest tseq))))

(rule *any [_ t]
  ;; consumes 1 token
  (->Token (first t) (rest t)))

(defn t
  ([v] (->Token v '()))
  ([v s] (->Token v s)))

(defn nil-fn [_] (identity nil))

(rule *ignore [op strm]
  (some-> (op strm) (assoc :value nil)))

(rule *eqStr [arg strm]
  (when (and
          (some? (first strm))
          (every? true? (map = (seq arg) strm)))
    (->Token arg (nthrest strm (count arg)))))

(rule *tostr [op strm]
  (let [res (op strm)
        chars (when res (map :value (:value res)))]
    (when (some? chars)
      (->T (String. (char-array chars)) (:stream res)))))

(rule *toInt [o strm]
  (let [res (o strm)]
    (when (some? res)
      (->T (BigInteger.
             ^String (:value res)) (:stream res)))))

;; work in progress
(rule *predStream [& arg strm] [[p strArg] arg]
  (map p strArg strm ))

