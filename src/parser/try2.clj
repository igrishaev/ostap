(ns parser.try2
  (:refer-clojure :exclude [compile]))


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


;;
;; StringParser
;;

(defrecord StringParser
    [string i?]

  IParser

  (-parse [_ chars]
    (let [[c & chars] chars]
      (if c
        (success c chars)
        (failure "EOF" "")))))


(defn make-string-parser [string options]
  (-> options
      (merge {:string string})
      (map->StringParser)))


(extend-protocol IParser

  clojure.lang.Symbol

  (-parse [this chars]
    (if-let [parser (get *definitions* this)]
      (-parse parser chars)
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


(defn parse-inner [sym chars]
  (-parse sym chars))


(defn parse [defs sym chars]
  (binding [*definitions* defs]
    (parse-inner sym chars)))


(def -spec
  '{some/parser
    ["foo" :aa 42]})


(def -defs
  (compile-defs -spec))

#_
(parse-main -defs 'some/parser "aaa")











#_
(defrecord GroupParser [parsers]

  IParser

  (-parse [_ chars]
    (loop [i 0
           acc []
           chars chars
           parsers parsers]
      (if-let [parser (first parsers)]
        (let [{:keys [success? data chars error?]}
              (-parse parser chars)]
          #_
          (cond
            success?
            (recur (inc i) (conj acc data) chars (next parsers))
            error?
            (failure "aaaa" (conj acc result))))
        #_
        (success acc chars)))))


#_
(defrecord ?Parser [parser]

  IParser

  #_
  (-parse [_ chars]
    (let [{:as result :keys [success? error?]}
          (-parse parser chars)]
      (cond
        success?
        (success data chars)


        )
      )

    )
  )