(ns json.grammar
  (:require; [regfunc.core :refer :all]
   [annotate.fns :refer :all]
   [annotate.types :refer :all]
   [taoensso.timbre.profiling :as prof])
  (:import [java.io InputStreamReader ByteArrayInputStream FileInputStream Reader BufferedReader]))

;; (token-fn) => #{nil, (->Token nil ..), (->Token some ..)}

(defrecord Token [value stream])

(declare *object)
(declare *array)

;; TODO rename to chunk or link
(defmulti get-token
  "given token or (raw) sequence, get the next Token, if any. See the
  tests for what you might consider strange behavior regarding
  differences in Token and sequence handling."
  type)

;;TODO utilize lazy stream
(defmethod get-token Token [t]
  (prof/p :**get-token-T (get-token (:stream t))))
(defmethod get-token clojure.lang.ISeq [s]
  (prof/p :**get-token-ISeq
    (when
        (seq s)
        (->Token (first s) (rest s)))))
(defmethod get-token nil [_])
(defmethod get-token :default [x] (throw (IllegalArgumentException. (str (type x)))))

(defn token-seq
  "if input is Token, it's value won't show up! the logic is that we are
  passed either raw sequence or token that is result of previous match
  so it's value is not needed."
  [t]
  (->> (get-token t)
       (iterate get-token)
       (take-while (comp not nil? :value))))

;;----------------
(defn file-to-seq [fname]
  (stream-to-seq
    (BufferedReader.
      (InputStreamReader.
        (FileInputStream. fname)))))

(defn stream-to-seq [^Reader isr]
  (take-while
   (comp not nil?)
   (map #(if (= -1 %) nil (char %))
        (repeatedly #(.read isr)))))

(defn txt [] (let [stream (-> "   123 456   789"
                              .getBytes
                              ByteArrayInputStream.
                              InputStreamReader.)]
               (stream-to-seq stream)))
;;---------------

(defn mod-t [^Token t fu]
  (when (some? t) (->Token (fu (:value t)) (:stream t))))

;; TODO there is problem with arrays - () is used internally so they
;; can't be represented with lists. currently I pack them in vectors
;; using the fact that vectors are not ISeq

(defn **lin [v]
  (if (seq? v)
    (prof/p :**lin-reduce
     (reduce
       (prof/fnp **lin-do [o v]
         (cond
           (seq? v) (concat o v)
           :else (concat o [v])))
       '() v))
    v))

(defn lin [fu]
  (fn [t] (mod-t (fu t) **lin)))

(defn *eq
  "matches using equality"
  [c]
  (let [cc c]
    (fn **eq [t]
      (let [tk (get-token t)]
        (when (= cc (:value tk)) tk)))))

(def *= *eq)

(defn *repeat [op]
  (prof/fnp **repeat [t]
    (let [res (->> (iterate op (op t))
                (take-while some?))]
      (when (seq res) (->Token (**lin (map :value res)) (:stream (last res)))))))

(defn *zero-or-more [op]
  "never returns nil as it's optional, however when there is no match the :value is null"
  (fn [t]
    (prof/p :zero-or-more
     (let [res ((*repeat op) t)]
       (if (nil? res)
         (->Token nil (if (some? (get-token t)) t '()))
         res)))))

(def ** *zero-or-more)

(def *one-or-more *repeat)
(def *+ *repeat)

(defn dump [lbl f]
  (fn [t]
    (let [r (f t)]
      (println "##" lbl ":" t "=>" r)
      r)))

(defn *group [& ops]
  (let [s-f (comp some? first)
        cnto (count ops)
        **it-fn (prof/fnp **it-fn [[to op]]
                  (when (and op to) [((first op) to) (rest op)]))]
    (fn [t]
      (prof/p :group
        (let [res (map
                    first
                    (take cnto
                      (take-while s-f
                        (iterate
                          **it-fn
                          [((first ops) t) (rest ops)]))))]
          (when (= cnto (count res))
            (->Token (filter some? (**lin (map :value res))) (:stream (last res)))))))))

(defn either [& ops]
  (fn [t]
    (prof/p :**either
     (reduce
       (fn [o v] (let [res (v t)]
                   (when (some? res) (reduced res))))
       nil
       ops))))

;;***********************

(defn *but [c]
  "the opposite of *eq. always exactly one token is taken from the stream"
  (let [eq (*= c)]
    (fn [t]
      (prof/p :**but
        (let [to (get-token t)]
          (when (not= c (:value to)) to)
                                        ;(when (nil? (eq t)) (get-token t))
          )))))

(defn *any []
  (prof/p :any
   (fn [t] (get-token t))))

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
              (fn [v] (when (some? v) (-> v tostr read-string num))))) ;; (num) is in in order to throw exception

(defn nil-fn [_] (identity nil))

(defn ignore [fu]
  (fn [t]
    (mod-t (fu t) nil-fn)))

(defn *pack [a]
  (fn [t]
    (mod-t (a t) vec)))

(def *esc-quot (prof/fnp *esc-quot [& a] (apply (*group (ignore (*eq \\)) (*eq \")) a)))

(def *string (*tostr
               (*group
                 (ignore (*eq \"))
                 (*zero-or-more
                   (either
                     *esc-quot
                     (*but \")))
                 (ignore (*eq \")))))

(def *whitespace
  (ignore (*zero-or-more (either (*eq \space) (*eq \return) (*eq \newline)))))

(def *number
  (*toint (*one-or-more (apply either (map *= '(\- \1 \2 \3 \4 \5 \6 \7 \8 \9 \0))))))

(def *true (*eq "true"))
(def *false (*eq "false"))
(def *null (*tostr (apply *group (map *= (seq "null")))))
(def *true (*tostr (apply *group (map *= (seq "true")))))
(def *false (*tostr (apply *group (map *= (seq "false")))))

(def *value (*group
              *whitespace
              (either
                *string
                *number
                #'*object
                #'*array
                *true
                *false
                *null
                )
              *whitespace
              ))

(def *array (*pack (*group
                     (ignore (*group *whitespace (*eq \[) *whitespace))
                     (*zero-or-more
                       (*group
                         ;; TODO automate this!!! causes problems, as it gets resolved to (def *array (... *array )) and that's not resolvable
                         #'*value
                         (*zero-or-more
                           (*group
                             (ignore(*= \,))
                             ;; TODO
                             #'*value))))
                     (ignore (*group *whitespace (*= \]) *whitespace)))))

(def *pair (*pack (*group *whitespace *string *whitespace (ignore (*= \:)) *whitespace #'*value)))

(def *object (*group
               *whitespace
               (ignore (*= \{))
               (*zero-or-more
                 (*group
                   #'*pair 
                   (*zero-or-more
                     (*group
                       (ignore (*= \,))
                       #'*pair))))
               *whitespace
               (ignore (*= \}))
               ))

(def *json (either *array *object))

