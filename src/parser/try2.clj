(ns parser.try2
  (:import java.util.ArrayList)
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
               (conj (if bind
                       `(let [~bind ~x-sym]
                          ~body)
                       `~body))))
         []
         (partition 2 pattern-body))


        cond-body
        (-> cond-body
            (conj :else)
            (conj `(throw (ex-info "No matching pattern found" {:x ~x-sym}))))]

    `(let [~x-sym ~x]
       (cond ~@cond-body))))


(defn correct-acc-string [options]
  (if (get options :string?)
    (assoc options :acc :string-builder :coerce 'str)
    options))


(defn add-acc-funcs [options]
  (merge
   options
   (case (get options :acc :vector)

     (string-builder :string-builder "string-builder")
     {:acc-new
      (fn []
        (new StringBuilder))
      :acc-add
      (fn [^StringBuilder sb x]
        (doto sb
          (.append x)))}

     (vector :vector "vector")
     {:acc-new
      (fn []
        [])
      :acc-add
      (fn [acc x]
        (conj acc x))}

     (array-list :array-list "array-list")
     {:acc-new
      (fn []
        (new ArrayList))
      :acc-add
      (fn [^ArrayList arr x]
        (doto arr
          (.add x)))})))


(def ^:dynamic *definitions* nil)


(defmulti -compile-vector
  (fn [[lead]]
    (cond
      (string? lead) :string
      (char? lead)   :char
      :else          lead)))


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


(defn supports-meta? [x]
  (instance? clojure.lang.IMeta x))


(defmacro as
  {:style/indent 1}
  [x [bind] & body]
  `(let [~bind ~x]
     ~@body))


(defn parse-inner [{:as parser
                    :keys [tag
                           meta
                           skip?
                           coerce
                           return]}
                   chars]

  (match (-parse parser chars)

    (Success {:as s :keys [data chars]})
    (if skip?

      (assoc s :skip? true)

      (let [[e data]
            (if coerce
              (try
                [nil (coerce data)]
                (catch Throwable e
                  [e data]))
              [nil data])]

        (if e
          (failure (format "Coercion error: %s" (ex-message e))
                   data)

          (cond-> data

            return
            (as [_]
              return)

            meta
            (as [x]
              (if (supports-meta? x)
                (with-meta x meta)
                x))

            tag
            (as [x]
              [tag x])

            :finally
            (as [x]
              (success x chars))))))

    (Failure f)
    f))


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

(defrecord GroupParser [parsers acc-new acc-add]

  IParser

  (-parse [this chars]

    (loop [i 0
           [parser & parsers] parsers
           acc (acc-new)
           chars chars]

      (if parser

        (match (parse-inner parser chars)

          (Success {:keys [data chars]})
          (recur (inc i) parsers (acc-add acc data) chars)

          (Failure f)
          (let [message
                (format "Parser %s failed" i)]
            (failure message (acc-add acc f))))

        (success acc chars)))))


(defn make-group-parser [parsers options]
  (-> options
      (merge {:parsers parsers})
      (correct-acc-string)
      (add-acc-funcs)
      (map->GroupParser)))


(defn -compile-group-impl [args]
  (let [[args-req args-opt]
        (split-args args)
        options
        (apply hash-map args-opt)]
    (make-group-parser (mapv compile-inner args-req) options)))


(defmethod -compile-vector 'group
  [[_ & args]]
  (-compile-group-impl args))


;;
;; ?Parser
;;

(defrecord ?Parser [parser]

  IParser

  (-parse [this chars]
    (match (parse-inner parser chars)
      (Success s) s
      (Failure f) (success nil chars))))


(defn make-?-parser [parser options]
  (-> options
      (merge {:parser parser})
      (map->?Parser)))


(defmethod -compile-vector '?
  [[_ parser & {:as options}]]
  (make-?-parser (compile-inner parser) options))


;;
;; +Parser
;;


(defrecord +Parser [parser acc-new acc-add]

  IParser

  (-parse [this chars]
    (match (parse-inner parser chars)

      (Success {:keys [data chars]})
      (loop [acc (acc-add (acc-new) data)
             chars chars]

        (match (parse-inner parser chars)
          (Success {:keys [data chars]})
          (recur (acc-add acc data) chars)

          (Failure f)
          (success acc chars)))

      (Failure f)
      (failure "+ error: the underlying parser didn't appear at least once"
               [f]))))


(defn make-+-parser [parser options]
  (-> options
      (merge {:parser parser})
      (correct-acc-string)
      (add-acc-funcs)
      (map->+Parser)))


(defmethod -compile-vector '+
  [[_ parser & {:as options}]]
  (make-+-parser (compile-inner parser) options))


;;
;; *Parser
;;

(defrecord *Parser [parser acc-new acc-add]

  IParser

  (-parse [this chars]
    (loop [acc (acc-new)
           chars chars]
      (match (parse-inner parser chars)
        (Success {:keys [data chars]})
        (recur (acc-add acc data) chars)
        (Failure f)
        (success acc chars)))))


(defn make-*-parser [parser options]
  (-> options
      (merge {:parser parser})
      (correct-acc-string)
      (add-acc-funcs)
      (map->*Parser)))


(defmethod -compile-vector '*
  [[_ parser & {:as options}]]
  (make-*-parser (compile-inner parser) options))


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


(defmethod -compile-vector 'or
  [[_ & args]]

  (let [[args-req args-opt]
        (split-args args)

        options
        (apply hash-map args-opt)]

    (make-or-parser (mapv compile-inner args-req) options)))


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


(defmethod -compile-vector 'join
  [[_ sep parser & {:as options}]]
  (make-join-parser (compile-inner sep) (compile-inner parser) options))


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


(defn resolve-func [sym]
  (or (some-> sym resolve deref)
      (throw (ex-info "Function not found" {:sym sym}))))


(defn compile-inner [x]
  (let [c (-compile x)]
    (cond-> c
      (:coerce c)
      (update :coerce resolve-func))))

;;
;; MM-compiler
;;

(defn split-args [form]
  (split-with (complement keyword?) form))


(defmethod -compile-vector :string
  [[string & {:as options}]]
  (make-string-parser string options))


(defmethod -compile-vector :char
  [[ch & {:as options}]]
  (make-string-parser (str ch) options))


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
     (assoc acc sym (compile-inner parser)))
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
  '{ws*
    [* [range "\r\n\t "] :skip? true]

    ws+
    [+ [range "\r\n\t "] :skip? true]

    word
    [+ [range [\a \z] [\A \Z] [\0 \9]]
     :string? true :tag word]

    value
    (ws* word ws+ word ws*)})


(def -defs
  (compile-defs -spec))


(defn parse-int [chars]
  (->> chars (apply str) (Integer/parseInt)))

#_
(parse -defs 'value "  abc   \t dbc  ")
