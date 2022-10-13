(ns parser.core
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io])
  (:import
   java.io.Reader
   java.util.Stack))


(defrecord Failure [message state]
  )

(defn failure [message state]
  (new Failure message state))


(defn failure? [x]
  (instance? Failure x))


(defprotocol IReader
  (read-char [this])
  (unread-char [this c])
  (unread-string [this string]))


(deftype MyReader [^Reader in
                   ^Stack stack]

  IReader

  (read-char [this]
    (if (.empty stack)
      (let [code (.read in)]
        (if (= -1 code)
          ::eof
          (char code)))
      (.pop stack)))

  (unread-char [this c]
    (.push stack c))

  (unread-string [this string]
    (doseq [c (str/reverse string)]
      (unread-char this c))))


(defn eof? [x]
  (identical? x ::eof))


(defn make-reader [in]
  (new MyReader in (new Stack)))


(defn make-reader-from-string [^String string]
  (-> string
      .getBytes
      io/reader
      make-reader))


(defn compile-dispatch
  [[lead & _]]
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
             reader]

  (let [result
        (fn-parse parser reader)]

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
         ^MyReader reader]

      (let [result
            (reduce
             (fn [^StringBuilder sb ^Character char-1]

               (let [char-2 (read-char reader)]

                 (if (eof? char-2)
                   (reduced (failure "EOF reached" (str sb)))

                   (if (if case-insensitive?
                         (= (Character/toLowerCase ^Character char-1)
                            (Character/toLowerCase ^Character char-2))
                         (= char-1 char-2))

                     (.append sb char-2)

                     (do
                       (unread-char reader char-2)
                       (unread-string reader (str sb))

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
    (fn [{:keys [parsers]} reader]
      (reduce
       (fn [acc [i parser]]
         (let [result
               (parse parser reader)]
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
    (fn [{:keys [parsers]} reader]
      (reduce
       (fn [_ parser]
         (let [result
               (parse parser reader)]
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
    (fn [{:keys [parser]} reader]
      (let [result
            (parse parser reader)]
        (if (failure? result)
          ::skip
          result)))}))


(defn make-range-parser [char-min char-max exclude options]
  (merge
   options
   {:type :range
    :char-min char-min
    :char-max char-max
    :exclude exclude
    :fn-parse
    (fn [{:keys [char-min char-max exclude]} ^MyReader reader]
      (let [char-in
            (read-char reader)]

        (if (eof? char-in)
          (failure "EOF reached" "")

          (if (and (<= (int char-min) (int char-in) (int char-max))
                   (not (contains? exclude char-in)))

            char-in
            (do
              (unread-char reader char-in)
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
    (fn [{:keys [parser]} reader]
      (let [result-first
            (parse parser reader)]
        (if (failure? result-first)
          (failure "+ error: the underlying parser didn't appear at least once" [result-first])
          (loop [acc [result-first]]
            (let [result
                  (parse parser reader)]
              (if (failure? result)
                acc
                (recur (conj acc result))))))))}))


#_
(defn make-string-of-parser [parser options]
  (merge
   options
   {:type :string-of
    :parser parser
    :fn-parse
    (fn [{:keys [parser]} reader]
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

  (def -r
    (make-reader-from-string "abBcXdd"))

  (parse -parser -r)


  )
