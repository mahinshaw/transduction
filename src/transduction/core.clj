(ns transduction.core)

;; REFERENCES
;;
;; http://clojure.org/reference/transducers
;;
;; https://www.youtube.com/watch?v=6mTbuzafcII => Strangeloop - watch first.
;;
;; https://www.youtube.com/watch?v=4KqUvG8HPYo => Clojure Conj
;;

;; What => Middleware for all your collection processing (Not the official definition).
;; Also "Process Transformations".
;; Why => O(n) > O(nm) => Single pass on collections. Similar to Java's coll.stream().map(...) or C#'s IEnumerable w/ `yield`.
;; How => Higher Order Functions, duh! (Functions that return functions).

;; Basic example
(def all-evens-inc-x-form (comp (filter even?) (map inc)))


(comment

  ;; Traditional methods of getting incremented even integers
  (->> (range 100000)
       (filter even?)
       (map inc))

  ((comp
    (partial map inc)
    (partial filter even?))
   (range 100000))

  (map inc (filter even? (range 100000)))

  (into [] (map inc (filter even? (range 100000))))

  (reduce + (map inc (filter even? (range 100000))))



  ;; transducer versions

  (sequence (comp (filter even?) (map inc)) (range 100000))

  (sequence all-evens-incremented (range 100000))

  (into [] all-evens-incremented (range 100000))

  ;; Returns a java.lang.Iterable, You may not ever need it. Maybe you will. I don't know.
  (eduction all-evens-incremented (range 100000))
  )


(comment

  (time (transduce all-evens-incremented + (range 10000000)))

  (time (reduce + 0 (map inc (filter even? (range 10000000))))))


;; Reimplement map and filter
(defn mapping [f]
  (fn [reducing-fn]
    (prn "Mapping received " reducing-fn) ;; lets look at what the reducing-fn really is
    (fn
      ;; Init
      ([] (reducing-fn))
      ;; Completion
      ([result] (reducing-fn result))
      ;; Step
      ([result input]
       (prn "Mapping calls " reducing-fn)
       (prn result input)
       (reducing-fn result (f input)))
      ([result input & inputs]
       (reducing-fn result (apply f input inputs))))))


(defn filtering [pred]
  (fn [reducing-fn]
    (prn "Filtering received " reducing-fn) ;; lets look at what the reducing-fn really is
    (fn
      ([] (reducing-fn))
      ([result] (reducing-fn result))
      ([result input]
       (prn "Filtering calls " reducing-fn)
       (prn result input)
       (if (pred input)
         (reducing-fn result input)
         result)))))


(defn taking [n]
  (fn [reducing-fn]
    (let [nv (volatile! n)]
      (fn
        ([] (reducing-fn))
        ([result] (reducing-fn result))
        ([result input]
         (let [n @nv
               nn (vswap! nv dec)
               result (if (pos? n)
                        (reducing-fn result input)
                        result)]
           (if (not (pos? nn))
             (ensure-reduced result)
             result)))))))
