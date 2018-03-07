(ns wam.ast-test
  (:require [clojure.test :refer :all]
            [wam.ast :refer [prolog-parser parse-query]])
  (:import [clojure.lang ExceptionInfo]))

(def query-text "p(Z, h(Z,W), f(W))")

(deftest parse-query-grammar
  (let [cst (prolog-parser query-text)]
    (is (= cst
           [:structure
            [:functor [:constant "p"]]
            [:subterm [:variable "Z"]]
            [:subterm [:structure
                       [:functor [:constant "h"]]
                       [:subterm [:variable "Z"]]
                       [:subterm [:variable "W"]]]]
            [:subterm [:structure
                       [:functor [:constant "f"]]
                       [:subterm [:variable "W"]]]]]))))

(deftest parse-failure
  (is (thrown? ExceptionInfo (parse-query "f")))
  (is (thrown? ExceptionInfo (parse-query "f()"))))

(deftest parse-to-ast
  (is (= (str (parse-query "f(a)"))
         "f/1(a)"))
  (is (= (str (parse-query "f(X)"))
         "f/1(X)"))
  (is (= (str (parse-query "f(g(a))"))
         "f/1(g/1(a))"))
  (is (= (str (parse-query "f(g(X))"))
         "f/1(g/1(X))"))
  (is (= (str (parse-query "f(g(a),b)"))
         "f/2(g/1(a), b)"))
  (is (= (str (parse-query "f(g(X),Y)"))
         "f/2(g/1(X), Y)"))
  (is (= (str (parse-query query-text))
         "p/3(Z, h/2(Z, W), f/1(W))")))
