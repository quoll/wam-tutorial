(ns ^{:doc "Type structures for WAM"
      :author "Paula Gearon"}
  wam.types
  (:require [clojure.spec.alpha :as s]))

(defrecord Structure [functor args])

(s/def ::functor string?)
(s/def ::args sequential?)

(s/def ::structure (s/and (partial instance? Structure)
                          (s/keys :req-un [::functor ::args])))

(s/def ::constant keyword?)
(s/def ::variable symbol?)
(s/def ::subterm (s/or ::constant ::variable))

(s/def ::ctype #{:str :ref})

(s/def ::heap (s/* (s/alt :functor (s/cat :name string? :arity int?)
                          :structure (s/cat :type ::ctype :index int?))))

