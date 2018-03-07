(ns wam.core-test
  (:require [clojure.test :refer :all]
            [wam.ast :refer :all]
            [wam.core :refer :all])
  (:import [wam.ast Structure Constant Variable]
           [wam.core Struct Ref]))

(def query-txt "p(Z, h(Z,W), f(W))")

(deftest reg-map-test
  (is (= (map-regs (parse-query "p(X)"))
         {:X1 (->Structure (->Constant "p") [:X2])
          :X2 (->Variable "X")}))
  (is (= (map-regs (parse-query "p(a,X)"))
         {:X1 (->Structure (->Constant "p") [:X2 :X3])
          :X2 (->Constant "a")
          :X3 (->Variable "X")}))
  (is (= (map-regs (parse-query "p(X,X)"))
         {:X1 (->Structure (->Constant "p") [:X2 :X2])
          :X2 (->Variable "X")}))
  (is (= (map-regs (parse-query "p(g(X))"))
         {:X1 (->Structure (->Constant "p") [:X2])
          :X2 (->Structure (->Constant "g") [:X3])
          :X3 (->Variable "X")}))
  (is (= (map-regs (parse-query "p(g(X),Y)"))
         {:X1 (->Structure (->Constant "p") [:X2 :X3])
          :X2 (->Structure (->Constant "g") [:X4])
          :X3 (->Variable "Y")
          :X4 (->Variable "X")}))
  (is (= (map-regs (parse-query "p(g(X),h(Y))"))
         {:X1 (->Structure (->Constant "p") [:X2 :X3])
          :X2 (->Structure (->Constant "g") [:X4])
          :X3 (->Structure (->Constant "h") [:X5])
          :X4 (->Variable "X")
          :X5 (->Variable "Y")}))
  (is (= (map-regs (parse-query query-txt))
         {:X1 (->Structure (->Constant "p") [:X2 :X3 :X4])
          :X2 (->Variable "Z")
          :X3 (->Structure (->Constant "h") [:X2 :X5])
          :X4 (->Structure (->Constant "f") [:X5])
          :X5 (->Variable "W")})))

(deftest term-order
  (let [term-order (comp order-query-terms map-regs parse-query)]
    (is (= (term-order "p(a)") [:X1]))
    (is (= (term-order "p(X)") [:X1]))
    (is (= (term-order "p(g(X))") [:X2 :X1]))
    (is (= (term-order "p(g(X),Y)") [:X2 :X1]))
    (is (= (term-order "p(Y,g(X))") [:X3 :X1]))
    (is (= (term-order query-txt) [:X3 :X4 :X1]))))

(deftest test-put-structure
  (let [fn-name (->Constant "f")
        struct-f2 (->Structure fn-name [:X2 :X3])
        store {:regs {} :heap []}
        store2 {:regs {} :heap [:a :b]}]
    (is (= (put-structure store struct-f2 :X1)
           {:regs {:X1 (->Struct 1)}
            :heap [(->Struct 1) [fn-name 2]]}))
    (is (= (put-structure store2 struct-f2 :X2)
           {:regs {:X2 (->Struct 3)}
            :heap [:a :b (->Struct 3) [fn-name 2]]}))))

(deftest test-add-query-structure
  (let [init-store {:regs {:X2 (->Ref 0)}
                    :heap [(->Ref 0)]}]
    (is
     (=
      (add-query-structure {:X1 (->Structure (->Constant "f") [:X2 :X3])}
                           :X1
                           init-store)
      {:regs {:X2 (->Ref 0)
              :X1 (->Struct 2)
              :X3 (->Ref 4)}
       :heap [(->Ref 0)
              (->Struct 2)
              [(->Constant "f") 2]
              (->Ref 0)
              (->Ref 4)]}))))
