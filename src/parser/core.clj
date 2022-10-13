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
                     mute?]}
             source]

  (let [result
        (fn-parse parser source)]

    (if (failure? result)
      result

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
  (merge
   options
   {:type :tuple
    :parsers parsers
    :fn-parse
    (fn [{:keys [parsers]} source]
      (reduce
       (fn [acc [i parser]]
         (let [result
               (parse parser source)]
           (if (failure? result)
             (let [message
                   (format "Error in tuple parser %s" i)]
               (reduced (failure message (conj acc result))))
             (conj acc result))))
       []
       (enumerate parsers)))}))


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


(defn make-range-parser [char-min char-max exclude options]
  (merge
   options
   {:type :range
    :char-min char-min
    :char-max char-max
    :exclude exclude
    :fn-parse
    (fn [{:keys [char-min char-max exclude]} ^Source source]
      (let [char-in
            (read-char source)]

        (if (eof? char-in)
          (failure "EOF reached" "")

          (if (and (<= (int char-min) (int char-in) (int char-max))
                   (not (contains? exclude char-in)))

            char-in
            (do
              (unread-char source char-in)
              (let [message
                    (format "Character %s doesn't match range %s-%s, exclude: %s"
                            char-in char-min char-max exclude)]
                (failure message char-in)))))))}))


(defn make-+-parser [parser options]
  (merge
   options
   {:type :+
    :parser parser
    :fn-parse
    (fn [{:keys [parser]} source]
      (let [result-first
            (parse parser source)

            state
            [result-first]]

        (if (failure? result-first)
          (failure "+ error: the underlying parser didn't appear at least once" state)
          (loop [acc state]
            (let [result
                  (parse parser source)]
              (if (failure? result)
                acc
                (recur (conj acc result))))))))}))


(defn make-+str-parser [parser options]
  (merge
   options
   {:type :+str
    :parser parser
    :fn-parse
    (fn [{:keys [parser]} source]
      (let [acc (new StringBuilder)

            result-first
            (parse parser source)]

        (.append acc result-first)

        (if (failure? result-first)

          (failure "+ error: the underlying parser didn't appear at least once" (str acc))

          (loop []
            (let [result
                  (parse parser source)]
              (if (failure? result)
                (str acc)
                (do
                  (.append acc result)
                  (recur))))))))}))



#_
(defn make-string-of-parser [parser options]
  (merge
   options
   {:type :string-of
    :parser parser
    :fn-parse
    (fn [{:keys [parser]} source]
      (let [sb (new StringBuilder)]

        ))}))


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


(defmethod compile '+str
  [[_ parser & {:as options}]]
  (make-+str-parser (compile parser) options))


#_
(defmethod compile 'string-of
  [[_ parser & {:as options}]]
  (make-string-of-parser (compile parser) options))



;; *
;; ws+
;; ws*
;; [tuple [ws*
;;         ["select" :case-insensitive? false]
;;         ws+

;;         [? [tuple [] :tag :from]]
;;         [? [tuple
;;             [ws*
;;              ["where" :case-insensitive? true]
;;              ws+
;;              [+ [range \a \z]]]
;;             :tag :where]]


;;         ["from" :case-insensitive? false]
;;         ws+
;;         [+ [range \a \z]]
;;         ws+
;;         ]]

(defmethod compile 'range
  [[_ [char-min char-max] & {:as options :keys [exclude]}]]
  (make-range-parser char-min char-max (set exclude) options))


(comment

  (def -spec
    '["AAA" :case-insensitive? true])

  (def -spec
    '[tuple
      [["aa" :case-insensitive? true]
       ["bb" :case-insensitive? true]
       ["cc" :case-insensitive? true]]])

  (def -spec
    '[or
      [["bb" :case-insensitive? true]
       ["aa" :case-insensitive? true]]])

  (def -spec
    '[tuple
      [["AA" :tag AAA :case-insensitive? true]
       [? ["xx"]]
       ["BB" :case-insensitive? true]]])

  (def -spec
    '[range [\0 \9] :exclude [\3]])

  (def -spec
    '[tuple [[+ [\a]] [+ [\b :case-insensitive? true]]]])

  (def -parser
    (compile -spec))

  (def -s
    (make-source-from-string "abcXdd"))

  (parse -parser -s)


  )
