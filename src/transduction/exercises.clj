(ns transduction.exercises)

;; Create a transducer that filters prime integers

(defn is-prime? [n]
  (.isProbablePrime (BigInteger/valueOf n) 5))

(def filter-primes (filter is-prime?))

;; Create a transducer that returns only fibonacci numbers

(def fibo-xform
  (fn [rf]
    (let [nm (volatile! [1 1])]
      (fn
        ([] (rf))
        ([result]
         (rf result))
        ([result input]
         (cond
           (= input 0) (rf result input)
           (= input 1) (do (rf result input) (rf result input))
           (= input (apply + @nm)) (do
                                     (vswap! nm (fn [x] [(second x) input]))
                                     (rf result input))
           :else (rf result)))))))

;; Calcalate the 13th prime fibonacci number (Just for fun).



;; Create a transducer that takes a header vector, with column names,
;; and a following of vectors with row data (like a csv),
;; and return maps where the keys are the column names,
;; and the values are the values for that column.

(defn mappify
  "Takes a sequence of row vectors, as commonly produced by csv parsing libraries, and returns a sequence of
  maps. By default, the first row vector will be interpreted as a header, and used as the keys for the maps.
  However, this and other behaviors are customizable via an optional `opts` map with the following options:
  * `:keyify` - bool; specify whether header/column names should be turned into keywords (default: `true`).
  * `:header` - specify the header to use for map keys, preventing first row of data from being consumed as header."
  ([] (mappify {}))
  ([{:keys [keyify header] :or {keyify true} :as opts}]
   (fn [rf]
     (let [make-header #(if keyify (mapv keyword %) %)
           hdr (volatile! (make-header header))]
       (fn
         ([] (rf))
         ([results] (rf results))
         ([results input]
          (if (empty? @hdr)
            (do (vreset! hdr (make-header input))
                results)
            (rf results (zipmap @hdr input)))))))))
