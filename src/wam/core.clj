(ns wam.core
  (:gen-class)
  (:require [wam.ast :as ast :refer [gen-term-reg normalize subterms? parse-query]]))

(defn map-regs
  "Creates a mapping of registers to terms"
  ([expr] (map-regs {} expr))
  ([regs expr]
   (let [id (inc (count regs))
         ;; map terms to fresh registers
         [new-regs nexpr _] (normalize expr (inc id) {})]
     ;; add in the root term, then reverse the mapping from term->reg to reg->term
     (->> (assoc new-regs nexpr (gen-term-reg id))
          (map (fn [[k v]] [v k]))
          (into {})))))

(defn order-query-terms
  "Orders terms for use in a query. This provides a bottom-up approach where a term
   does not appear until all of its arguments have been defined.
   Returns registers referring to terms with arguments, in query ordering."
  [reg-map]
  (let [;; a function to retrieve all the arguments for a term referred to by a register
        args-for (comp :terms reg-map)
        ;; find all registers that are defined without depending on other terms
        var-registers (->> reg-map
                           (remove (comp subterms? second))
                           (map first)
                           set)
        ;; register names are unordered. Get the registers that use arguments in name order
        ordered-keys (->> (count reg-map)
                          range
                          (map (comp gen-term-reg inc))
                          (remove var-registers))]
    ;; check that the ordered registers really all have terms with arguments
    (assert (every? (comp subterms? reg-map) ordered-keys)
            (str "Bad-keys: " (into [] ordered-keys)))
    ;; accumulate registers from the front, which have all of their arguments defined
    (loop [seen-regs var-registers acc [] remaining ordered-keys]
      (if-not (seq remaining)
        acc
        (let [all-seen? #(if (every? seen-regs (args-for %)) %) 
              next-reg (some all-seen? remaining)]
          ;; ensure that at least one terms has all arguments defined.
          (assert next-reg (str "No trace found for vars in: " (map reg-map remaining)))
          (recur (into (conj seen-regs next-reg) (args-for next-reg))
                 (conj acc next-reg)
                 (remove #{next-reg} remaining)))))))

(defrecord Struct [r])

(defrecord Ref [r])

(defn put-structure
  [{:keys [regs heap] :as store} {:keys [function terms] :as structure} termr]
  (let [heap-top (count heap)
        new-struct (->Struct (inc heap-top))
        new-heap (-> heap
                     (conj new-struct)
                     (conj [function (count terms)]))
        new-regs (assoc regs termr new-struct)]
    {:regs new-regs :heap new-heap}))

(defn add-query-structure
  "Adds a term with arguments to the heap, updating the registers as needed."
  [reg->term termr store]
  ;; look up the term register
  (let [s (reg->term termr)
        ;; and ensure that it's a structure to add
        _ (assert (instance? wam.ast.Structure s))
        arguments (:terms s)]
    (letfn [(choose-set-fn [{:keys [regs heap] :as store} arg-reg]
              (if-let [arg (regs arg-reg)]
                (update store :heap conj arg)
                (let [unbound-ref (->Ref (count heap))]
                  (-> store
                      (update :heap conj unbound-ref)
                      (update :regs assoc arg-reg unbound-ref)))))]
      (-> store
          (put-structure s termr)
          ((partial reduce choose-set-fn) arguments)))))

(defn build-query-heap
  "Build a heap structure to describe the query."
  [reg->term struct-regs]
  (loop [[term-reg & rregs] struct-regs store {:regs {} :heap []}]
    (if-not term-reg
      store
      (recur rregs (add-query-structure reg->term term-reg store)))))

(defn qcompile 
  [text]
  (let [ast (parse-query text)
        reg->term (map-regs ast)
        flattened-form (order-query-terms reg->term)
        query-heap (build-query-heap reg->term flattened-form)]
    query-heap))

(defn -main
  "Compile a program"
  [& args]
  (println "Hello, World!"))
