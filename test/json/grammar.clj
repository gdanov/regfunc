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

(deffragment *value (*ctor (comp :value first) ;; splice the group...why is that necesery? it's spliced once already
             (*group
               *whitespace
               (*either
                 *string
                 *number
                 #'*object
                 #'*array
                 *true
                 *false
                 *null
                 )
               *whitespace)
             #_((fn [to] (let [v (some-> to :value first)
                             s (some-> to :stream)]
                         (some-> v (assoc :stream s)))))))

(deffragment *closing-bracket (*ignore (*group *whitespace (*eq \]) *whitespace)))

(deffragment *array
  (*ctor (fn [v] (some->> v (map :value) vec))
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
        *closing-bracket))))

(deffragment *pair
  (*ctor 
    (fn [r] (when r { (-> r first :value) (-> r last :value)}))
    (*group *whitespace *string *whitespace (*ignore (*eq \:)) #'*value)))

(deffragment *object
  (*ctor (fn [res] (when res (reduce merge {} (map :value res))))
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
      (*ignore (*eq \})))))

(deffragment *json (*either *array *object))



