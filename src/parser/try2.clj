(ns parser.try2
  (:refer-clojure :exclude [compile]))


(defmacro match
  {:style/indent 1}
  [x & pattern-body]
  (let [x-sym
        (gensym "x")

        cond-body
        (reduce
         (fn [acc [[Record bind] body]]
           (-> acc
               (conj `(instance? ~Record ~x-sym))
               (conj `(let [~bind ~x-sym]
                        ~body))))
         []
         (partition 2 pattern-body))


        cond-body
        (-> cond-body
            (conj :else)
            (conj `(throw (ex-info "No matching pattern found" {}))))]

    `(let [~x-sym ~x]
       (cond ~@cond-body))))


(def ^:dynamic *definitions* nil)


;;
;; Failure
;;

(defrecord Failure [message state]

  Object

  (toString [this]
    (format "<<%s>>" message)))


(defn failure [message state]
  (new Failure message state))


(defn failure? [x]
  (instance? Failure x))


;;
;; Success
;;

(defrecord Success [data chars])


(defn success [data chars]
  (new Success data chars))


(defn success? [x]
  (instance? Success x))


;;
;; Parser
;;

(defprotocol IParser
  (-parse [this chars]))


(defn parse-inner [sym chars]
  (-parse sym chars))


;;
;; StringParser
;;

(defrecord StringParser
    [string i?]

    IParser

    (-parse [_ chars]

      (loop [sb (new StringBuilder)
             string string
             chars chars]

        (let [[ch1 & string] string
              [ch2 & chars] chars]

          (cond

            (nil? ch1)
            (success (str sb) chars)

            (nil? ch2)
            (failure "EOF reached" (str sb))

            (if i?
              (= (Character/toLowerCase ^Character ch1)
                 (Character/toLowerCase ^Character ch2))
              (= ch1 ch2))
            (recur (.append sb ch2) string chars)

            :else
            (failure
             (format "Expected %s but got %s, i?: %s"
                     ch1 ch2 i?)
             (str sb)))))))


(defn make-string-parser [string options]
  (-> options
      (merge {:string string})
      (map->StringParser)))


(extend-protocol IParser

  clojure.lang.Symbol

  (-parse [this chars]
    (if-let [parser (get *definitions* this)]
      (parse-inner parser chars)
      (failure (format "No definition found for parser %s" this) nil))))


;;
;; MM-compiler
;;

(defmulti -compile-vector
  (fn [[lead]]
    (cond
      (string? lead) :string
      (char? lead)   :char
      :else          lead)))


(defmethod -compile-vector :string
  [[string & {:as options}]]
  (make-string-parser string options))


(defmethod -compile-vector :char
  [[ch & {:as options}]]
  (make-string-parser (str ch) options))


;;
;; Compiler
;;


(defprotocol ICompiler
  (-compile [this]))


(extend-protocol ICompiler

  String

  (-compile [this]
    (make-string-parser this nil))

  Character

  (-compile [this]
    (make-string-parser (str this) nil))

  clojure.lang.Symbol

  (-compile [this]
    (if-let [parser (get *definitions* this)]
      (-compile parser)
      (throw (ex-info
              (format "No definition found for parser %s" this)
              {:parser this}))))

  clojure.lang.PersistentVector

  (-compile [this]
    (-compile-vector this)))


(defn compile-defs [spec]
  (reduce-kv
   (fn [acc sym parser]
     (assoc acc sym (-compile parser)))
   {}
   spec))


(defn parse [defs sym chars]
  (binding [*definitions* defs]

    (match (parse-inner sym chars)
      (Success {:keys [data]})
      data

      (Failure {:as f :keys [message]})
      (throw (ex-info (format "Parsing failure: %s" message)
                      {:failure f :symbol sym})))))


(def -spec
  '{char/aaa
    ["aaa" :i? false]

    char/bbb
    ["bbb" :i? false]

    some/parser
    ["aaaaaaa" :i? false]})


(def -defs
  (compile-defs -spec))

#_
(parse -defs 'some/parser "AAAAAAAAAAAAAAAAA")
