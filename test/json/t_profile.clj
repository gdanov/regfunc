(ns json.t-profile
  (:require [json.grammar :as gr :refer :all :exclude [one-or-more parse]]
            [regfunc.macro :refer :all]
            [regfunc.core :refer :all]
            [midje.sweet :refer :all :exclude [one-of anything]]
            [taoensso.timbre.profiling :as prof]))

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

(def benchmarks [(prof/fnp transient-vec []
                   (doall
                     (repeatedly 10
                       (fn []
                         (count
                           (last
                             (take 100
                               (iterate #(conj! % 1) (transient [])))))))))
                 
                 (prof/fnp persistent-vec []
                   (doall
                     (repeatedly 10
                       (fn []
                         (count
                           (last
                             (take 100
                               (iterate #(conj % 1) []))))))))

                 (prof/fnp new-token []
                   (dotimes [n 100] (->Token n '(1 2 3 4 5 6))))
                 
                 (prof/fnp t.repeat []
                   (dotimes [n 100]) (doall (repeat 10 "a")))
                 
                                        ; (prof/fnp tostr.char []
                                        ; (dotimes [n 1000] (tostr \c)))
                 
                                        ; (prof/fnp tostr.seq.char []
                                        ; (dotimes [n 100] (tostr '(\a \a \a \a \a \a \a \a \a \a ))))

                 (prof/fnp ***repeat-either-eq []
                   (dotimes [n 100] ((fragment (*repeat (*either (*eq 1) (*eq 2)))) '(1 2 1 2 1 2 1 2 1 2))))
                                  
                 (prof/fnp ***repeat-eq []
                   (dotimes [n 100] ((fragment (*repeat (*eq 1))) '(1 1 1 1 1 1 1 1 1 1))))
                 
                 (prof/fnp ***eq []
                   (dotimes [n 1000] ((fragment (*eq nil)) '(nil))))

                 (prof/fnp **== []
                   (dotimes [n 1000] ((fn blaa [arg] (->T (= arg arg))) n)))
                 
                 (prof/fnp ***string []
                   (dotimes [n 100] (*string nil '(\" \a \a \a \a \a \a \a \a \a \a \"))))
                 
                 (prof/fnp t.read-string.int []
                   (dotimes [n 100] (num (read-string "12345"))))
                 
                 (prof/fnp tostr.String.ctor []
                   (let [se '(\a \a \a \a \a \a \a \a \a \a )]
                     (dotimes [n 100] (String. (char-array se)))))
                 
                                        ; (prof/fnp tostr.seq.take.repeat.str []
                                        ; (dotimes [n 100] (tostr (take 10 (repeat "a")))))
                 
                                        ; (prof/fnp tostr.seq.repeat.str []
                                        ; (dotimes [n 100] (tostr (repeat 10 "a"))))
                 
                                        ; (prof/fnp tostr.seq.take.repeat.char []
                                        ; (dotimes [n 100] (tostr (take 10 (repeat \a)))))
                 
                                        ; (prof/fnp tostr.seq.repeat.char []
                                        ; (dotimes [n 100] (tostr (repeat 10 \a))))
                 
                 (prof/fnp t.rest []
                   (let [_sequence '(1 2 3 4 5 6 7 8 9 10 11)]
                     (dotimes [n 1000] (rest _sequence))))
                 
                 (prof/fnp t.next []
                   (let [_sequence '(1 2 3 4 5 6 7 8 9 10 11)]
                     (dotimes [n 1000] (next _sequence))))
                 
                 (prof/fnp t.doall []
                   (let [_sequence '(1 2 3 4 5 6 7 8 9 10 11)]
                     (dotimes [n 100] (doall (seq _sequence)))))
                 
                 (prof/fnp t.take []
                   (dotimes [n 100] (doall (take 10 (iterate inc 1)))))
                 
                 (prof/fnp t.take-while.iterate []
                   (dotimes [n 100] (doall (take-while #(< % 10) (iterate inc 1)))))
                 
                 (prof/fnp t.take-while.seq []
                   (let [_sequence '(1 2 3 4 5 6 7 8 9 10 11)]
                     (dotimes [n 100] (doall (take-while #(< % 10) _sequence)))))
                 
                 (prof/fnp t.take-while.vector []
                   (let [_sequence [1 2 3 4 5 6 7 8 9 10 11]]
                     (dotimes [n 100] (doall (take-while #(< % 10) _sequence)))))
                 
                 (prof/fnp t.loop []
                   (dotimes [n 100] (loop [c 0 co (iterate inc 1) res []]
                                      (if (< c 10)
                                        (recur (inc c) (next co) (conj res (first co)))
                                        res))))

                 (prof/fnp t.loop.transient []
                   (dotimes [n 100] (loop [c 0 co (iterate inc 1) res (transient [])]
                                      (if (< c 10)
                                        (recur (inc c) (next co) (conj! res (first co)))
                                        res))))

                 (prof/fnp t.for []
                   (let [src (iterate inc 1)]
                     (dotimes [n 100] (doall (for [x src
                                                   :while (< x 10)] x)))))
                 (prof/fnp t.reduce []
                   (dotimes [n 100] (reduce
                                      (fn [o v]
                                        (if (< v 10)
                                          (conj o v)
                                          (reduced o)))
                                      []
                                      (iterate inc 1))))
                 
                 (prof/fnp t.reduce.transient []
                   (dotimes [n 100] (reduce
                                      (fn [o v]
                                        (if (< v 10)
                                          (conj! o v)
                                          (reduced o)))
                                      (transient [])
                                      (iterate inc 1))))
                 ])

(defn run-bench []
  (doall
    (dotimes [n 1000] (doall (map (fn [f] (f)) benchmarks)))))

#_(prof/profile :info :benchmarks
 (run-bench))
