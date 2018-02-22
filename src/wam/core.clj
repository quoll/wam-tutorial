(ns wam.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [wam.types :refer :all]
            [instaparse.core :as insta]))

(def grammar-file "grammar.bnf")

(insta/defparser qpp (io/resource grammar-file))

(defn read-struct
  [ast]
  )

(defn struct-subterm?
  "tests is a subterm is a structure"
  [term]
  (when (sequential? term)
    (let [[f s] term]
      (and (= :subterm f)
           (sequential? s)
           (= :structure (first s))))))

(s/fdef read-struct
  :args (s/cat :struct-kw #{:structure}
               :terms (s/* :wam.types/term))
  :ret (s/coll-of :wam.types/structure)
  :fn (< (count (:ret %))
         (count (filter struct-subterm? (:args %)))))

(defn qcompile 
  [text]
  (let [ast (qpp text)
        structure (read-struct ast)
        ordered (order-structs structure)]
    (build-heap ordered)))

(s/fdef qcompile
  :args (s/cat :text string?)
  :ret :wam.types/heap
  :fn true)


(defn -main
  "Compile a program"
  [& args]
  (println "Hello, World!"))
