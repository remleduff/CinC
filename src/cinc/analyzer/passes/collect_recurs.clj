(ns cinc.analyzer.passes.collect-recurs
  (:require [cinc.analyzer.utils :refer [protocol-node? update!]]
            [cinc.analyzer.passes :refer [walk-with-path *ast-path*]]))

(def ^:private ^:dynamic *loop-collects*
  {:loop-recur-paths           {}})

(defn -collect-recurs
  [{:keys [op loop-locals] :as ast}]
  (case op
    :recur
    (do
      (prn *ast-path*)
      (doall
       (map-indexed
        (fn [idx item]
          (update! *loop-collects* update-in [:loop-recur-paths (:name item)]
               (fnil conj [])
                    *ast-path*))
          loop-locals))
      ast)

    ast)
  )

(defn collect-recurs
  [{:keys [op env] :as ast}]
  (if (= :loop op)
    (binding [*loop-collects* *loop-collects*]
      (into (walk-with-path ast -collect-recurs identity)
            *loop-collects*))
    ast))
