(ns regfunc.macro
  (:require [clojure.zip :as zip]
            [clojure.test :refer :all]))

(defrecord Token [value stream])

(defn name-key [fname]
  (keyword
    (name (ns-name *ns*))
    (name fname)))

(defn ***splice [res#]
  (if (and (seq? res#) (seq res#))
    (mapcat
      (fn splice-mapper [v#]
        (if (seq? (:value v#))
          (:value v#)
          (list v#)))
      res#)
    res#))

(defn splice-result [res]
  (if (and
        (instance? Token res)
        (some? (:value res)))
    (-> res
      :value
      ***splice
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

(def ^:dynamic ttt)
(def ^:dynamic path)
(def last-fail  (atom []))
(def last-match (atom nil))

;; nill reporting - walking the syntax tree left -> right depth first. reset last fail after success because we are sure the fail did not terminate the evaluation (it was in an or)

(defn record-nil
  "middleware that records path data when nil is returned in order to
  help with partial match heuristics when matching fails"
  [res]
  ;; TODO get the stream
  (if (nil? res)
    (do
      (when (= 1 (count (var-get #'path)))
        ;(println "nil from" (and (bound? #'path) (var-get #'path)) ":" @last-fail "\n ->" @last-match)
        )
      (swap! last-fail (fn [v]
                         (if (> (count (var-get #'path)) (count v))
                           (do
           ;                  (println "!!!!" @last-match (var-get #'path) v)
                             (var-get #'path))
                           v)))
                                        ; (println "nil from" (var-get #'ttt) @last-fail) 
      nil)
    (do

      (if (and
            (some? (:value res)) ;; pesky *ignore :(
            (not (instance? Token (:value res))))
        (do
          ;; reset only when leaf was reached. don't reset at intermediate success like redundant whitespace rule match
          (reset! last-fail [])
          (reset! last-match (:stream res))
          ;(println "reset" (var-get #'path) res)
          ))
      res)))

(def result-postprocessors
  [;`record-nil
   ; 'dump-res
   'remove-stream
   ; 'dump-res
   'remove-nil-vals
   ; 'dump-res
   'splice-result
   ; 'dump-res
   ]
  ) 

(defn rule-macro?
  "(*eq 1) => false as it would evaluete to rule fn
  *eq => true"
  [exp]
  (and ;; *eq ; not sure what's the use of this
    (symbol? exp)
    (-> exp resolve meta ::rule))
  )

(defn rule-fn?
  "intended to be used on unevaluated forms by macros"
  [exp]
  (or
    (and ;; **eq-one
      (symbol? exp)
      (some-> exp resolve var-get meta ::rule.fn))
    (and ;; (*eq 1) -> macro that will evaluate to fn
      (seq? exp)
      (symbol? (first exp))
      (-> exp first resolve meta ::rule))
    (and
      (fn? exp)
      (-> exp meta ::rule.fn))))

(defn wrap-fn [exp]
;(if rule-macro? exp)
  exp)

(defn gen-body [sub-name variadic? p1sym p1val p2sym defctx bindings body]
  `(binding [ttt (conj [] #_(if (bound? #'ttt) (var-get #'ttt) []) '~sub-name)]
     ;; we heavily rely on the p1val being expanded bellow in the same thread
     (let [~p1sym ~(if variadic?
                     `(list ~@(map wrap-fn p1val))
                     ;;
                     (wrap-fn p1val))
           ~@bindings]
       (with-meta
         ;; the parametrized fn | **eq
         (fn ~sub-name
           ([~p2sym] (~sub-name ~defctx ~p2sym))
           ([~'ctx ~p2sym]
            (binding [path nil #_(conj (if (bound? #'path) (var-get #'path) [])
                             (if ~(some? (::path-fn (meta sub-name)))
                               (~(if (some? (::path-fn (meta sub-name))) (::path-fn (meta sub-name)) 'identity) ~p1sym)
                               '~sub-name))]
                                        ; (println "path#" ~(meta sub-name) path)
              (-> (do
                   ~@body)
                ~@result-postprocessors))))
         {::rule.fn true
          ;; TODO
          ::rule.name "todo"}))))

;; lifecycle stages
;; rule definition (rule xx ...) -> (macro *xx ...) -> this is the rule macro
;; rule parametrization (*xx ...) -> (fn *xx# ...) -> parametrized rule fn
;; rule execution ((*xx ...) <stream>)

;;TODO rule to throw exception when called outside of fragment

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
        sub-name (with-meta (symbol (str "*" fname)) (meta fname))
        namesym (symbol (name (ns-name *ns*)) (name fname))
        _body (next args)
        bindings (when (vector? (first _body)) (first _body))
        body (if (vector? (first _body)) (rest _body) _body)
        result-postprocessors '(identity)
        ;; the rule macro | *eq
        template `(defmacro ~fname {::rule true}
                    ([~'ctx ;; must be always called inside fragment def
                      ~@p1]
                                                  ;; things that need to shadow the params or add "static" values
                                                  ;; :pre-sub-fn
                                                  (gen-body
                                                    '~sub-name
                                                    ~(= '& (first p1))
                                                    '~(last p1)
                                                    ~(last p1)
                                                    '~p2
                                                    ~'ctx
                                                    '~bindings
                                                    '~body)))]
    (eval template)))

;; (deffragment (*eq 1)) =>  (*eq {} 1)
;; (deffragment (*group (*eq 1) (*eq 2)) => (*group {} (fragment (*eq 1)) (fragment (*eq 2)
;; (deffragment (*group *whitespace (*eq 1)) => ;*whitespace is fragment
;; (*group {} *whitespace (fragment (*eq 1)))
;; (deffragment *eq 1) => should never happen

;; op must *evaluate* to parametrized fn. possible values:
;; - (^rule-macro *eq 2)
;; - ^parametrized-rule-fn whitespace
;; output must be respectively
;; - (partial (*eq 2) {}) or? (*eq ^ctx {} 2)
;; - (partial whitespace {})
;; result evaluates to
;; (fn [ctx stream] ...)
(defn- fragment*
  "form can be (*eq 1) or *one"
  [ctx form] 
  (assert (-> form rule-fn?) (str "form does not evaluate to parametrized rule function:" form))

  (let [context (update-in ctx [:path] conj
                  (cond
                       (some? (:name ctx)) (:name ctx)
                       (seq? form) `'~(first form)
                       :else `'~form))]
    (if (seq? form)
     `(~(first form)
       ~context
       ~@(map
           (fn [t] (if (rule-fn? t) `(fragment ~context ~t) t))
           (rest form)))
     `(partial ~form ~context))))

(defmacro fragment
  "the anonymous fn"
  ([ctx form] (fragment* ctx form))
  ([form] (fragment* {:path []} form)))

(defmacro deffragment
  "binds symbol at macroextension"
  [nam form]
  ;; TODO symbols in path are without namespace. is that how I want it?
  (eval `(def ~nam ~(fragment* {:path [] :name `'~nam} form))))


(defn parse [r strm]
  (identity;binding [last-fail-path nil]
    (let [res (r strm)]
      (when (nil? res) (println "!nomatch!" ;last-fail-path
                         ))
      res)))
