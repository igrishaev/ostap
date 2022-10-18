(ns parser.try2
  (:refer-clojure :exclude [compile]))


(def SKIP ::skip)


(defn skip? [x]
  (identical? x SKIP))


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


(defn parse-inner [parser chars]
  (-parse parser chars))


;;
;; StringParser
;;

(defrecord StringParser
    [string i?]

  IParser

  (-parse [_ chars]

    (loop [sb (new StringBuilder)
           [ch1 & string] string
           [ch2 & chars] chars]

      (cond

        (nil? ch1)
        (success (str sb) (cons ch2 chars))

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
         (str sb))))))


(defn make-string-parser [string options]
  (-> options
      (merge {:string string})
      (map->StringParser)))


;;
;; GroupParser
;;

(defrecord GroupParser [parsers]

  IParser

  (-parse [this chars]

    (loop [i 0
           [parser & parsers] parsers
           acc []
           chars chars]

      (if parser

        (match (parse-inner parser chars)

          (Success {:keys [data chars]})
          (recur (inc i) parsers (conj acc data) chars)

          (Failure f)
          (let [message
                (format "Parser %s failed" i)]
            (failure message (conj acc f))))

        (success acc chars)))))


(defn make-group-parser [parsers options]
  (-> options
      (merge {:parsers parsers})
      (map->GroupParser)))


;;
;; ?Parser
;;


(defrecord ?Parser [parser]

  IParser

  (-parse [this chars]
    (match (parse-inner parser chars)
      (Success s)
      s
      (Failure f)
      (success SKIP chars))))


(defn make-?-parser [parser options]
  (-> options
      (merge {:parser parser})
      (map->?Parser)))


;;
;; +Parser
;;

(defrecord +Parser [parser options]

  IParser

  (-parse [this chars]

    (match (parse-inner parser chars)
      (Success {:keys [data chars]})
      (loop [acc [data]
             chars chars]
        (match (parse-inner parser chars)
          (Success {:keys [data chars]})
          (recur (conj acc data) chars)

          (Failure f)
          (success acc chars)))

      (Failure f)
      (failure "+ error: the underlying parser didn't appear at least once"
               [f]))))


(defn make-+-parser [parser options]
  (-> options
      (merge {:parser parser})
      (map->+Parser)))


;;
;; *Parser
;;

(defrecord *Parser [parser options]

  IParser

  (-parse [this chars]
    (loop [acc []
           chars chars]
      (match (parse-inner parser chars)
        (Success {:keys [data chars]})
        (recur (conj acc data) chars)
        (Failure f)
        (success acc chars)))))


(defn make-*-parser [parser options]
  (-> options
      (merge {:parser parser})
      (map->*Parser)))


;;
;; OrParser
;;

(defrecord OrParser [parsers options]

  IParser

  (-parse [this chars]
    (loop [[parser & parsers] parsers]
      (if parser
        (match (parse-inner parser chars)
          (Success s)
          s
          (Failure f)
          (recur parsers))
        (failure "All the parsers have failed" "")))))


(defn make-or-parser [parsers options]
  (-> options
      (merge {:parsers parsers})
      (map->OrParser)))


;;
;; Symbol compiler
;;

(extend-protocol IParser

  clojure.lang.Symbol

  (-parse [this chars]
    (if-let [parser (get *definitions* this)]
      (parse-inner parser chars)
      (failure (format "No definition found for parser %s" this) nil))))


(defprotocol ICompiler
  (-compile [this]))

;;
;; MM-compiler
;;


(defn split-args [form]
  (split-with (complement keyword?) form))


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


(defmethod -compile-vector '>
  [[_ & args]]
  (let [[args-req args-opt]
        (split-args args)

        options
        (apply hash-map args-opt)]

    (make-group-parser (mapv -compile args-req) options)))


(defmethod -compile-vector '?
  [[_ parser & {:as options}]]
  (make-?-parser (-compile parser) options))


(defmethod -compile-vector 'or
  [[_ & args]]

  (let [[args-req args-opt]
        (split-args args)

        options
        (apply hash-map args-opt)]

    (make-or-parser (mapv -compile args-req) options)))


(defmethod -compile-vector '+
  [[_ parser & {:as options}]]
  (make-+-parser (-compile parser) options))


(defmethod -compile-vector '*
  [[_ parser & {:as options}]]
  (make-*-parser (-compile parser) options))


;;
;; Compiler
;;

(extend-protocol ICompiler

  String

  (-compile [this]
    (make-string-parser this nil))

  Character

  (-compile [this]
    (make-string-parser (str this) nil))

  clojure.lang.Symbol

  (-compile [this]
    this)

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
      (Success {:keys [data chars]})
      data

      (Failure {:as f :keys [message]})
      (throw (ex-info (format "Parsing failure: %s" message)
                      {:failure f :symbol sym})))))


(def -spec
  '{char/aaa
    ["aaa" :i? false]

    char/bbb
    ["BBB" :i? true]

    char/ccc
    "ccc"

    some/parser
    [> char/aaa [? char/bbb] char/ccc]

    some/foo
    [or
     "XXX"
     some/parser]

    some/test+
    [+ \a]

    some/test*
    [* \a]})


(def -defs
  (compile-defs -spec))

#_
(parse -defs 'some/parser "aaaccc")
