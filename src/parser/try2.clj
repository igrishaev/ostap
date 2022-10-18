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

(defrecord +Parser [parser]

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

(defrecord *Parser [parser]

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

(defrecord OrParser [parsers]

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
;; JoinParser
;;

(defrecord JoinParser [sep parser]

  IParser

  (-parse [_ chars]

    (match (parse-inner parser chars)
      (Success {:keys [data chars]})
      (loop [acc [data]
             chars chars]
        (match (parse-inner sep chars)
          (Success {:keys [data chars]})
          (match (parse-inner parser chars)
            (Success {:keys [data chars]})
            (recur (conj acc data) chars)
            (Failure f)
            (failure "Join error: parser has failed after separator" (conj acc f)))
          (Failure f)
          (success acc chars)))
      (Failure f)
      (failure "Join error: the first item is missing" [f]))))


(defn make-join-parser [sep parser options]
  (-> options
      (merge {:sep sep
              :parser parser})
      (map->JoinParser)))


;;
;; RangeParser
;;

(def conj-set
  (fnil conj #{}))


(def conj-into
  (fnil into #{}))


(defn collect-range-args [args]

  (loop [acc nil
         pos? true
         args args]

    (let [[arg & args] args]

      (cond

        (nil? arg)
        acc

        (= '- arg)
        (recur acc false args)

        (char? arg)
        (recur (update-in acc [pos? :chars] conj-set arg) true args)

        (string? arg)
        (recur (update-in acc [pos? :chars] conj-into (seq arg)) true args)

        (vector? arg)
        (recur (update-in acc [pos? :ranges] conj-set arg) true args)

        :else
        (recur acc true args)))))


(defn char-in-range? [c [c-min c-max]]
  (<= (int c-min) (int c) (int c-max)))


(defrecord RangeParser [chars-neg
                        ranges-neg
                        chars-pos
                        ranges-pos]

  IParser

  (-parse [this chars]

    (if-let [c (first chars)]

      (let [allowed?
            (if (or (contains? chars-neg c)
                    (when ranges-neg
                      (some (partial char-in-range? c) ranges-neg)))
              false
              (if (or chars-pos ranges-pos)
                (or (contains? chars-pos c)
                    (when ranges-pos
                      (some (partial char-in-range? c) ranges-pos)))
                true))]

        (if allowed?
          (success c (rest chars))
          (let [message
                (with-out-str
                  (printf "Character %s is not allowed for this parser. " c)
                  (when (or chars-pos ranges-pos)
                    (print "The allowed characters are ")
                    (doseq [c chars-pos]
                      (print c))
                    (doseq [[c-min c-max] ranges-pos]
                      (printf "[%s-%s]" c-min c-max)))
                  (print \.)
                  (when (or chars-neg ranges-neg)
                    (print "The disallowed characters are ")
                    (doseq [c chars-neg]
                      (print c))
                    (doseq [[c-min c-max] ranges-neg]
                      (printf "[%s-%s]" c-min c-max))))]
            (failure message c))))
      (failure "EOF reached" ""))))


(defn make-range-parser [chars-pos chars-neg ranges-pos ranges-neg options]
  (-> options
      (merge {:chars-pos chars-pos
              :chars-neg chars-neg
              :ranges-pos ranges-pos
              :ranges-neg ranges-neg})
      (map->RangeParser)))


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


(defn -compile-group-impl [args]
  (let [[args-req args-opt]
        (split-args args)
        options
        (apply hash-map args-opt)]
    (make-group-parser (mapv -compile args-req) options)))


(defmethod -compile-vector '>
  [[_ & args]]
  (-compile-group-impl args))


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


(defmethod -compile-vector 'join
  [[_ sep parser & {:as options}]]
  (make-join-parser (-compile sep) (-compile parser) options))


(defmethod -compile-vector 'range
  [[_ & args]]

  (let [[args-req args-opt]
        (split-args args)

        options
        (apply hash-map args-opt)

        args-map
        (collect-range-args args-req)

        chars-pos
        (some-> args-map (get true) :chars not-empty)

        chars-neg
        (some-> args-map (get false) :chars not-empty)

        ranges-pos
        (some-> args-map (get true) :ranges not-empty)

        ranges-neg
        (some-> args-map (get false) :ranges not-empty)]

    (make-range-parser chars-pos chars-neg ranges-pos ranges-neg options)))


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

  clojure.lang.PersistentList
  (-compile [args]
    (-compile-group-impl args))

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
    [* \a]

    test/join
    [join \, ["xxx" :i? false]]

    test/foo2
    [or
     (\{ ws \} :return {})
     (\{ [join \, json/keyval] \})]

    test/abc
    [range [\0 \9] -[\3 \5] -\9]

    test/foo
    [>
     "aaa"
     [or
      [join \, ["xxx" :i? false]]
      [join \, ["yyy" :i? false]]]
     [or "bbb" "ccc"]]})


(def -defs
  (compile-defs -spec))

#_
(parse -defs 'some/parser "aaaccc")
