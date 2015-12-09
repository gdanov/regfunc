(ns regfunc.core
  (:require [clojure.string :as str]
            [schema.core :as sc]
            [taoensso.timbre.profiling :as prof]))

(defn vectorize [v]
  (cond
    (vector? v) v

    (or (list? v) (seq? v)) (into [] v)

    :else [v]))

(def !nil? (comp not nil?))

(def filter-nils
  (partial filter !nil?))

(defprotocol ParserNode
  (parse [this lines]))

(sc/defschema ParserNodeT
  (sc/protocol ParserNode))

(defprotocol Result
  (matches? [this])
  (result [this])
  (remaining-lines [this])

  (partial-matches? [this])
  (location [this]))

(sc/defschema ResultT (sc/protocol Result))

(defrecord ResultR [match? result lines]
  Result
  (matches? [this] match?)
  (result [this] result)
  (remaining-lines [this] lines)

  (partial-matches? [this] (and (not (matches? this)) (:partial this)))
  (location [this] (:location this)))

(defn not-matches? [v]
  (not (matches? v)))

(defn mk-match [result lines]
  (->ResultR true result lines))

(defn mk-nil-match [lines]
  (->ResultR true nil lines))

(defn mk-nomatch
  ([lines] ;; TODO do I really need the lines when there is no match????
   (->ResultR false nil lines))
  
  ([res lines]
   (->ResultR false res lines)))

(defn mk-partial-match [location result lines]
  (map->ResultR {:match? false
                 :partial true
                 :location location
                 :result result
                 :lines lines}
    ))

(defn mk-result [match result lines] 
  (->ResultR match result lines))

(sc/defschema ProcessorResultSc
  (sc/conditional                     ; [type] matches both [] and nil
    #(or
       (some? %)
       (seq %))                         ; does not make senes
    (sc/either
      ResultT
      [ResultT])))

(defn parse* [fun lines]
  (fun lines))

(defn mk-pn
  "make processor/parser node"
  ([fun]
   (mk-pn :put-an-id-here fun))

  ([id fun]
   (let [pf (fn [lines]
              (prof/p (keyword (str "mk-pn-" (name id)))
                (parse* fun lines)))
           
         res (reify
               ParserNode
               (parse [this lines] (pf lines))

               clojure.lang.IFn
               (invoke [this lines] (pf lines))
               (applyTo [this lines] (pf lines))
               (toString [this] (str "== parser node " (type fun) " =="))
               )]
     res
     )))


;; funs beginning with $ are output-related, not
;; parsing-related. These operations do not affect the matching logic.

(sc/defn $ignore :- ParserNodeT 
  "Filters out the result of fun, the matching is delegated to fun.
must be used, as we need the grammar to understand each and every
token. (tag) directly inside ($node) is ignored by default."
  
  [fun :- ParserNodeT]
  (mk-pn :ignore
    (fn [lines]
      (let [res (fun lines)]
        (mk-result
          (matches? res)
          nil
          (remaining-lines res)))))
  )

(sc/defn $node :- ParserNodeT
  "produce a tree node represented as map.
handles metadata *only* for first-level
trds. Mathcing behaviour equivalent to (group). ($node (maybe ($child (tag 'A')))) is not guaranteed to work."

  ;; TODO rewrite to reuse group matching logic
  
  [& trds :- [ParserNodeT]]
  (mk-pn :node
    (sc/fn :- ResultT [lines :- [String]]
      (reduce
        (fn [fn-res fun]
          (if (matches? fn-res)
            (let [_r (fun (remaining-lines fn-res))
                  _rer (result _r)]
              (mk-result
                (matches? _r)
                (cond
                  (:root (meta fun))
                  (assoc (result fn-res) :root _rer)

                  (:child (meta fun))
                  (update-in (result fn-res) [:child]
                    #(concat (or % []) (vectorize _rer)))

                  (and (:val (meta fun)) _rer)
                  (assoc (result fn-res) (-> fun meta :val) _rer)

                  (:merge (meta fun))
                  (merge (result fn-res) _rer)
                  
                  :else
                  (result fn-res))
                (remaining-lines _r)))
            (reduced fn-res)))
        (mk-match {} lines)
        trds))))

(defn $merge
  "fun is ParserNode. effectively does (merge parent-node result-of-fun)"
  [fun]
  (vary-meta fun assoc :merge true))
 
(defn $val
  "tag is keyword. fun is ParserNode. to be used to add named children to $node. effectively does (assoc parent-node tag result-of-fun). same as ($merge {tag value-of-fun})"
  [tag fun]
  (vary-meta fun assoc :val tag))

(defn $root
  "same as ($val :root fun)"

  [fun]
  (vary-meta fun assoc :root true))

(defn $child
  "almost the same as ($val :child fun). More than one $child per $node is possible."
  [fun]
  (vary-meta fun assoc :child true))

(sc/defn one-of :- ParserNodeT [& options :- [ParserNodeT]]
  (mk-pn :one-of
    (sc/fn :- ResultT [lines :- [sc/Any]]
      (or
        (reduce
          (fn [o v] (when (matches? v) (reduced v)))
          nil (map #(% lines) options))
        (mk-nomatch lines)))))

(sc/defn maybe :- ParserNodeT [e :- ParserNodeT]
  (mk-pn :maybe
    (fn  [lines]
      (let [res (e lines)]
        (if (matches? res)
          res
          (mk-nil-match lines))))))

(sc/defn zero-or-more :- ParserNodeT

  [fun :- ParserNodeT]
  
  (mk-pn :zero-or-more
    (fn [lines]
      (last
        (take-while
          #(matches? %)
          (iterate
            (fn [res]
              (let [r (fun (remaining-lines res))]
                (mk-result
                  (matches? r)
                  (filter some?
                    (into
                      (vectorize (or (result res) []))
                      (vectorize (result r))))
                  (remaining-lines r))))
            (mk-match nil lines)))))))

(defn apply-fns
  "applies the functions one after another, piping them until all are
  applied and match or there is no-match?  when there was no match
  only the non-matching result is returned"

  [fns lines]
  (rest                             ; get rid of the first fake result
    (reduce
      (fn [o v]
        (let [res (v (remaining-lines (last o)))]
          (if (matches? res)
            (conj o res)
            (reduced ["aeu" res])))) 
      [(mk-nil-match lines)]
      fns)))

(sc/defn group :- ParserNodeT
  "acts as sequence : [form1 form2 ...]. Matching sematnics? (every? matches? %)?"

  [& funs :- [ParserNodeT]]
  (mk-pn :group
    (fn [lines]
      (let [results (apply-fns (flatten funs) lines)
            partial-match   (first (filter not-matches? results))]
        (cond
          partial-match
          (mk-partial-match
            (concat ["group"] (vectorize (location partial-match)))
            nil
            (remaining-lines partial-match))

          (or (empty? results)
            ;; premature optimization, but why not...it's redundant
            ;; anyway
            (not-matches? (last results))) 
          (mk-nomatch [] lines)

          :else
          (mk-match
            (->> (apply concat (map (comp vectorize result) results))
              (filter some?)
              (#(if (seq (rest %)) % (first %))))
            (remaining-lines (last results)))
          )))))

;; what's better (repeat (maybe (tag))) or (maybe (repeat (tag))) ? 
;; effectively both are possible, but the first gets you into infinite loop

(sc/defn one-or-more :- ParserNodeT
  "acts as '+'. For '*' use (maybe (one-or-more ...))"

  [fun :- ParserNodeT]
  (mk-pn :one-or-more
    (fn [lines]
      (reduce
        (fn [o v]
          (if (matches? v)
            (mk-match (concat (result o) (vectorize (result v))) (remaining-lines v))
                                        ;else
            (reduced o)))
        (mk-nomatch [] lines)
        (iterate
          #(fun (remaining-lines %))
          (fun lines))))))



(sc/defn anything :- ParserNodeT
  [& opts]
  (let [{:keys [stop capture?] :or {stop (mk-pn :anything1 (fn [lines] (mk-nomatch [])))}} opts]
   (mk-pn :anything2
     (fn [lines]
       (if (and
             (seq lines)
             (not-matches? (stop lines)))
         (mk-match
           (when capture? (first lines))
           (rest lines))
         (mk-nomatch nil lines))))))

;; ******** testing utilities !nil?

(defn result-is-nil [r]
  (-> r result nil?))

(defn result-is-not-nil [r]
  (not (result-is-nil r)))

(defn result-is [v]
  (fn [r] (and
            (= (result r) v)
            (matches? r))))


(defn parse-file [gram fname]
  (let [lines (->> (slurp fname)
                (clojure.string/split-lines))
        res (gram lines)
        r-lines (remaining-lines res)
        linenum (- (count lines) (count r-lines))
        ]

    (if (empty? r-lines)
      ;; (clojure.pprint/pprint (result res))
      res
      {:lines (take 5 r-lines)
       :line# linenum})))

