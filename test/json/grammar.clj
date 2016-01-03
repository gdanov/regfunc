(ns json.grammar
  (:require [regfunc.macro :refer :all]
            [regfunc.core :refer :all])) 

(declare *object)
(declare *array)

(deffragment *esc-quot (*group (*ignore (*eq \\)) (*eq \")))

(deffragment *string (*tostr
                       (*group
                         (*ignore (*eq \"))
                         (*splice (*zero-or-more
                                    (*splice (*either
                                               (*repeat (*pred (comp not #{\\ \"})))
                                               *esc-quot))))
                         (*ignore (*eq \")))))

(deffragment *whitespace
  (*ignore (*zero-or-more (*pred #{\space \return \newline}))))

(deffragment *number
  (*toInt (*tostr (*repeat (*pred #{\- \1 \2 \3 \4 \5 \6 \7 \8 \9 \0})))))

(deffragment *null (*eqStr "null"))
(deffragment *true (*either (*eqStr "true") (*eqStr "True") (*eqStr "TRUE")))
(deffragment *false (*either (*eqStr "false") (*eqStr "False") (*eqStr "FALSE")))

(def *value (fn [strm]
              (some-> strm
                ((fragment
                   (*group
                     *whitespace
                     (*splice
                       (*either
                         *string
                         *number
                         #'*object
                         #'*array
                         *true
                         *false
                         *null
                         ))
                     *whitespace)))
                ((fn [to] (let [v (some-> to :value first)
                                s (some-> to :stream)]
                            (some-> v (assoc :stream s))))))))

(deffragment *closing-bracket (*ignore (*group *whitespace (*eq \]) *whitespace)))

(def *array (fn [strm]
              (let [res ((fragment
                           (*group
                             (*ignore (*group *whitespace (*eq \[) *whitespace))
                             (*either
                               (*group
                                 #'*value
                                 (*either
                                   *closing-bracket
                                   (*group
                                     (*repeat
                                       (*group
                                         (*ignore (*eq \,))
                                         ;; TODO
                                         #'*value))
                                     *closing-bracket)))
                               *closing-bracket)))
                         strm)]
                (some->> (:value res)
                  (map :value)
                  vec
                  (#(->T % (:stream res)))))))

(def *pair (fn [strm]
             (-> strm
               ((fragment
                  (*group *whitespace *string *whitespace (*ignore (*eq \:)) #'*value)))
               ((fn [r]
                  (when r (->T {(:value (first (:value r))) (:value (last (:value r)))} (:stream r))))))))

(def *object (fn [strm]
               (let [res (-> strm
                           ((fragment
                              (*group
                                *whitespace
                                (*ignore (*eq \{))
                                (*zero-or-more
                                  (*group
                                    #'*pair 
                                    (*zero-or-more
                                      (*group
                                        (*ignore (*eq \,))
                                        #'*pair))))
                                *whitespace
                                (*ignore (*eq \}))))))]
                 (when res (->T (reduce merge  {} (map :value (:value res))) (:stream res))))))

(def *json (*either *array *object))



