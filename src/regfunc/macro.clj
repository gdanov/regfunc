(ns regfunc.macro
  (:require [clojure.zip :as zip]))

(defrecord Token [value stream])

(defn name-key [fname]
  (keyword
    (name (ns-name *ns*))
    (name fname)))

(defn splice-result [res]
  (if (and
        (instance? Token res)
        (some? (:value res)))
    (-> res
      :value
      ((fn ***splice [res#]
         (if (and (seq? res#) (seq res#))
           (mapcat
             (fn [v#]
               (if (seq? (:value v#))
                 (:value v#)
                 (list v#)))
             res#)
           res#)))
      (->Token (:stream res)))
    res))

(defn remove-nil-vals [res]
  (if (seq? (:value res))
    (->Token (filter (comp some? :value) (:value res)) (:stream res))
    res))

(defn remove-stream [res]
  (cond
    (instance? Token (:value res)) (assoc res :stream '())
    (and (seq? (:value res)) (instance? Token (first (:value res)))) (->Token (map (fn [v] (assoc v :stream '())) (:value res)) (:stream res))
    :else res))

(defn dump-res [r]
  ; (println r)
  r)

(def result-postprocessors
  ['dump-res
   'remove-stream
   'dump-res
   'remove-nil-vals
   'dump-res
   'splice-result
   'dump-res]
  ) 

(def ^:dynamic ttt)

(defn gen-body [sub-name p1 p1val p2 bindings body]
  `(binding [ttt (conj (if (bound? #'ttt) (var-get #'ttt) []) '~sub-name)]
     ;; we heavily rely on the p1val being expanded bellow in the same thread
     (println "~~" '~sub-name "~~" (and (bound? #'ttt) (var-get #'ttt)))
     (let [~(last p1) ~(if (= '& (first p1)) (into [] p1val) p1val)
           ~@bindings]
       (fn ~sub-name [~p2]
         (some->
           ~@body
           ~@result-postprocessors)))))

(defmacro rule
  {:arglits '([fname [& p1-ops p2-strm] [bindings*]? body])
   :doc "produces macro with the shape (defmacro name [fqn? & p1] (let [bindings?] body). When that macro is called with ops, anonimous funtction is produced with the shape (fn *name [p2] ...) and advices/middlewares applied."}
  [fname & args                         ; arglist & _body
   ]
  (let [ ;; stupid me. this belongs in the body of the generated macro/function
        ;;        fqn (when (::rule-fqn (meta (first args))) (first args))
        arglist (first args)
        p1 (butlast arglist)
        p2 (last arglist)
                                        ; _ (println "defrule" fname arglist p1 p2)
        sub-name (symbol (str "*" fname))
        namesym (symbol (name (ns-name *ns*)) (name fname))
        _body (next args)
        bindings (when (vector? (first _body)) (first _body))
        body (if (vector? (first _body)) (rest _body) _body)
        result-postprocessors '(identity)
        template `(defmacro ~fname [~@p1]
                    ;; things that need to shadow the params or add "static" values
                    ;; :pre-sub-fn
                    (gen-body '~sub-name '~p1 ~(last p1) '~p2 '~bindings '~body))]
    template))

(defmacro deffragment
  {:arglists '([name fqn? op])
   :doc "fqn is list of the symbols of the predecessors. label is
   keyword."}
  [name & args]
  
  (let [fqn (when (::rule-fqn (meta (first args))) (first args))
        ops (as-> (if (some? fqn) (rest args) args) res
              (loop [col res
                     argmap {}]
                (if (seq col)
                  (if (keyword? (first col))
                    ;; TODO validation & sane key generation when no label is provided
                    (recur (nnext col) (assoc argmap (first col) (fnext col)))
                    (recur (rest col) (assoc argmap (print-str (first col)) (first col))))
                  argmap)))
        types (map
                (fn [[k o]]
                  (if (list? o)
                    (let [f (first o)
                          fsymres (resolve f)
                          fvar (var-get fsymres)]
                      ["-------\n" (print-str f) (type f) (fn? fvar) "\nMsexp:" (meta o) "\nMvar:" (meta fvar) "\nMsym:" (meta fsymres)])
                    (type o)))
                ops)]
    (println "deffragment =>>" fqn "\n^" (meta name))
    (doall (map println types))
    (doall (map println ops))
    `(defn ~name [strm#]
       ~@(map (fn mapopsfn [[l o]] (if (keyword? l) `(~(first o) ~l ~@(rest o)) o)) ops)
       )))
