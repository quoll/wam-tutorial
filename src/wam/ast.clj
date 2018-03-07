(ns wam.ast
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [instaparse.core :as insta])
  (:import [instaparse.gll Failure]))

;; contains the Prolog grammar
(def grammar-file "prolog.bnf")

;; creates a CST parser
(insta/defparser prolog-parser (io/resource grammar-file))

;; Defines terms that are a present in a Prolog expression
(defprotocol Term
  (normalize [term next-reg-id regs]
    "Accepts a term to add to registers, along with the next ID that may be used for sub terms,
     and the registers so far.
     Return new registers that have been updated with subterms,
     the normalized term, and the next id that may be used.")
  (subterms? [term] "Returns true if this contains sub terms"))

(defn gen-term-reg
  "Generate a term register name for an id"
  [reg-id]
  (keyword (str "X" reg-id)))

(defrecord Structure [function terms]
  Object
  (toString [_] (str (:label function) "/" (count terms) "("
                     (str/join ", " terms) ")"))
  Term
  (normalize [this next-reg-id regs]
    (let [[new-regs nts new-next]
          (loop [regs regs
                 id next-reg-id
                 next-sub-id (+ (count (remove regs terms)) next-reg-id)
                 normalized-terms []
                 [term & rterms] terms]
            (if-not term
              [regs normalized-terms next-sub-id]
              (let [[r nt nsi] (normalize term next-sub-id regs)
                    [upd-reg term-reg ni] (if-let [sr (r nt)]
                                            [r sr id]
                                            (let [new-sr (gen-term-reg id)]
                                              [(assoc r nt new-sr) new-sr (inc id)]))]
                (recur upd-reg ni nsi (conj normalized-terms term-reg) rterms))))]
      [new-regs (->Structure function nts) new-next]))
  (subterms? [_] true))

(defrecord Constant [label]
  Object
  (toString [_] label)
  Term
  (normalize [this next-reg-id regs] [regs this next-reg-id])
  (subterms? [_] false))

(defrecord Variable [label]
  Object
  (toString [_] label)
  Term
  (normalize [this next-reg-id regs] [regs this next-reg-id])
  (subterms? [_] false))

(defmulti read-struct
  "Converts a CST element into an AST Term element. Recursively descends the CST.
   A CST element is defined as a sequence containing a type tag, and the arguments." first)

(defmethod read-struct :structure
  [[_ & ast]]
  (let [[f & terms] (map read-struct ast)]
    (->Structure f terms)))

(defmethod read-struct :functor
  [[_ cnst]]
  (read-struct cnst))

(defmethod read-struct :subterm
  [[_ v]]
  (read-struct v))

(defmethod read-struct :constant
  [[_ label]]
  (->Constant label))

(defmethod read-struct :variable
  [[_ label]]
  (->Variable label))

(defn parse-query
  "Parses the text of a Query into an AST."
  [text]
  (let [cst (prolog-parser text)]
    (if-not (instance? Failure cst)
      (read-struct cst)
      (throw (ex-info "Parse error" cst)))))
