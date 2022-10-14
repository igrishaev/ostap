(ns parser.core
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io])
  (:import
   java.io.Reader
   java.util.Stack))


(defrecord Failure [message state]

  Object

  (toString [this]
    (format "<<%s>>" message)))

(defn failure [message state]
  (new Failure message state))


(defn failure? [x]
  (instance? Failure x))


(def EOF ::eof)

(defn eof? [x]
  (identical? x EOF))

(def SKIP ::skip)

(defn skip? [x]
  (identical? x SKIP))


(defn make-acc-funcs [acc-type]
  (case acc-type

    :vec
    [(constantly [])
     (fn [coll x]
       (if (skip? x)
         coll
         (conj coll x)))
     identity]

    :str
    [(fn []
       (new StringBuilder))

     (fn [^StringBuilder sb x]
       (if (skip? x)
         sb
         (.append sb x)))

     str]))

(defn split-args [form]
  (split-with (complement keyword?) form))


(defprotocol ISource
  (read-char [this])
  (unread-char [this c])
  (unread-string [this string]))


(deftype Source [^Reader in
                 ^Stack stack]

  ISource

  (read-char [this]
    (if (.empty stack)
      (let [code (.read in)]
        (if (= -1 code)
          EOF
          (char code)))
      (.pop stack)))

  (unread-char [this c]
    (.push stack c))

  (unread-string [this string]
    (doseq [c (str/reverse string)]
      (unread-char this c))))


(defn make-source [in]
  (new Source in (new Stack)))


(defn make-source-from-string [^String string]
  (-> string
      .getBytes
      io/reader
      make-source))


(defn compile-dispatch [[lead & _]]
  (cond
    (string? lead) :string
    (char? lead) :char
    :else lead))


(defmulti compile compile-dispatch)


(defn supports-meta? [x]
  (instance? clojure.lang.IMeta x))


(defn parse [{:as parser
              :keys [fn-parse
                     fn-coerce
                     meta
                     tag
                     skip?]}
             source]

  (let [result
        (fn-parse parser source)]

    (cond

      (failure? result)
      result

      skip?
      SKIP

      :else
      (let [result
            (if fn-coerce
              (fn-coerce result)
              result)

            result
            (if (and meta (supports-meta? result))
              (with-meta result meta)
              result)

            result
            (if tag
              ^:tag [tag result]
              result)]

        result))))


(defn string-builder? [x]
  (instance? StringBuilder x))


(defn make-string-parser [string options]
  (merge
   options
   {:type :string
    :string string
    :fn-parse
    (fn [{:keys [string
                 case-insensitive?]}
         ^Source source]

      (let [result
            (reduce
             (fn [^StringBuilder sb ^Character char-1]

               (let [char-2 (read-char source)]

                 (if (eof? char-2)
                   (reduced (failure "EOF reached" (str sb)))

                   (if (if case-insensitive?
                         (= (Character/toLowerCase ^Character char-1)
                            (Character/toLowerCase ^Character char-2))
                         (= char-1 char-2))

                     (.append sb char-2)

                     (do
                       (unread-char source char-2)
                       (unread-string source (str sb))

                       (reduced (failure
                                 (format "String parsing error: expected %s but got %s, case-i flag: %s"
                                         char-1 char-2 case-insensitive?)
                                 (str sb))))))))

             (new StringBuilder)
             string)]

        (if (string-builder? result)
          (str result)
          result)))}))


(defn enumerate [coll]
  (map-indexed vector coll))


(defn make-tuple-parser [parsers options]

  (let [[acc-new
         acc-add
         acc-fin]
        (make-acc-funcs (get options :acc :vec))]

    (merge
     options
     {:type :tuple
      :parsers parsers
      :fn-parse
      (fn [{:keys [parsers]} source]

        (loop [i 0
               parsers parsers
               acc (acc-new)]

          (if-let [parser (first parsers)]

            (let [result
                  (parse parser source)

                  acc
                  (acc-add acc result)]

              (if (failure? result)

                (let [message
                      (format "Error in tuple parser %s" i)]
                  (failure message (acc-fin acc)))

                (recur (inc i) (next parsers) acc)))

            (acc-fin acc))))})))


(defn make-or-parser [parsers options]
  {:pre [(seq parsers)]}
  (merge
   options
   {:type :or
    :parsers parsers
    :fn-parse
    (fn [{:keys [parsers]} source]
      (reduce
       (fn [_ parser]
         (let [result
               (parse parser source)]
           (if (failure? result)
             result
             (reduced result))))
       nil
       parsers))}))


(defn make-?-parser [parser options]
  (merge
   options
   {:type :?
    :parser parser
    :fn-parse
    (fn [{:keys [parser]} source]
      (let [result
            (parse parser source)]
        (if (failure? result)
          SKIP
          result)))}))


(defn make-+-parser [parser options]

  (let [[acc-new
         acc-add
         acc-fin]
        (make-acc-funcs (get options :acc :vec))]

    (merge
     options
     {:type :+
      :parser parser
      :fn-parse
      (fn [{:keys [parser]} source]

        (let [acc
              (acc-new)

              result-first
              (parse parser source)

              state
              (acc-add acc result-first)]

          (if (failure? result-first)

            (failure "+ error: the underlying parser didn't appear at least once"
                     (acc-fin state))

            (loop [acc state]
              (let [result
                    (parse parser source)]

                (if (failure? result)
                  (acc-fin acc)
                  (recur (acc-add acc result-first))))))))})))


(defn make-*-parser [parser options]

  (let [[acc-new
         acc-add
         acc-fin]
        (make-acc-funcs (get options :acc :vec))]

    (merge
     options
     {:type :*
      :parser parser
      :fn-parse
      (fn [{:keys [parser]} source]

        (let [acc
              (acc-new)

              result-first
              (parse parser source)

              state
              (acc-add acc result-first)]

          (if (failure? result-first)
            SKIP
            (loop [acc state]
              (let [result
                    (parse parser source)]
                (if (failure? result)
                  (acc-fin acc)
                  (recur (acc-add acc result-first))))))))})))


;; ;; times
;; [times 4 \a]
;; [times [3 6] \a]

;; [times 3]
;; [times [2]]
;; [times [2 4]]


(defn make-times-parser [times parser options]

  (let [[acc-new
         acc-add
         acc-fin]
        (make-acc-funcs (get options :acc :vec))

        [times-min times-max]
        (cond
          (int? times)
          [times times]
          (vector? times)
          times
          :else
          (throw (ex-info "Wrong times argument" {:times times})))]

    (merge
     options
     {:type :times
      :parser parser
      :times-min times-min
      :times-max times-max
      :fn-parse
      (fn [_ source]

        #_
        (loop [n 0
               acc (acc-new)]

          (let [result
                (parse parser source)]

            (if (failure? result)

              yes

              (if (= n (if (nil? times-max)
                         times-min
                         times-max))

                finish
                next)))))})))


(defmethod compile :times
  [[_ times parser & {:as options}]]
  (make-times-parser times (compile parser) options))


(defmethod compile :string
  [[string & {:as options}]]
  (make-string-parser string options))


(defmethod compile :char
  [[c & {:as options}]]
  (make-string-parser (str c) options))


(defmethod compile 'tuple
  [[_ parsers & {:as options}]]
  (make-tuple-parser (mapv compile parsers) options))


(defmethod compile 'or
  [[_ parsers & {:as options}]]
  (make-or-parser (mapv compile parsers) options))


(defmethod compile '?
  [[_ parser & {:as options}]]
  (make-?-parser (compile parser) options))


(defmethod compile '+
  [[_ parser & {:as options}]]
  (make-+-parser (compile parser) options))


(defmethod compile '*
  [[_ parser & {:as options}]]
  (make-*-parser (compile parser) options))


(defmethod compile 'times
  [[_ n parser & {:as options}]]
  (make-times-parser n (compile parser) options))



(defn collect-range-args [args]
  (reduce
   (fn [acc arg]
     (cond

       (char? arg)
       (update acc :chars conj arg)

       (string? arg)
       (update acc :chars into (seq arg))

       (vector? arg)
       (update acc :ranges conj arg)

       :else
       (throw (ex-info "Wrong range argument" {:arg arg}))))

   {:chars #{}
    :ranges #{}}
   args))


(defn char-in-range? [c [c-min c-max]]
  ;; TODO: use compare
  (<= (int c-min) (int c) (int c-max)))



(defn fn-range-parser
  [{:keys [chars-pos
           ranges-pos
           chars-neg
           ranges-neg]}
   ^Source source]

  (let [char-in
        (read-char source)]

    (if (eof? char-in)
      (failure "EOF reached" "<EOF>")

      (let [allowed?
            (if (or (contains? chars-neg char-in)
                    (when ranges-neg
                      (some (partial char-in-range? char-in) ranges-neg)))
              false
              (if (or chars-pos ranges-pos)
                (or (contains? chars-pos char-in)
                    (when ranges-pos
                      (some (partial char-in-range? char-in) ranges-pos)))
                true))]

        (if allowed?
          char-in
          (do
            (unread-char source char-in)
            (let [message
                  (with-out-str
                    (printf "Character %s is not allowed for this parser. " char-in)
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
              (failure message char-in))))))))


(defn make-range-parser
  [args-pos args-neg options]

  (let [{chars-pos :chars
         ranges-pos :ranges}
        (collect-range-args args-pos)

        {chars-neg :chars
         ranges-neg :ranges}
        (collect-range-args args-neg)]

    (merge
     options
     {:type :range
      :chars-pos (not-empty chars-pos)
      :ranges-pos (not-empty ranges-pos)
      :chars-neg (not-empty chars-neg)
      :ranges-neg (not-empty ranges-neg)
      :fn-parse fn-range-parser})))



(defmethod compile 'range
  [[_ & args-raw]]

  (let [[args-req args-opt]
        (split-args args-raw)

        options
        (apply hash-map args-opt)

        [args-pos args-neg]
        (split-with (complement #{'- -}) args-req)

        args-neg
        (next args-neg)]

    (make-range-parser args-pos args-neg options)))






(def ws+
  '[+ [or [["\r"] ["\n"] ["\t"] [\space]]] :acc :str :skip? true])

(def ws*
  '[* [or [["\r"] ["\n"] ["\t"] [\space]]] :acc :str :skip? true])

(def digit
  '[range [\0 \9]])


(comment

  (def -spec
    '[range [\a \z] [\0 \9] "ABC" - "_" [\3 \5]])

  (def -parser
    (compile -spec))

  (def -s
    (make-source-from-string "4"))

  (parse -parser -s)


  )
