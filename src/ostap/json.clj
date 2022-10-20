(ns ostap.json
  (:require
   [clojure.java.io :as io]
   [ostap.core :as o]))

(def grammar
  '{ws
    [* [range \return \newline \tab \space] :skip? true]

    hex
    [range [\0 \9] [\a \f] [\A \F]]

    comma
    (ws \, ws :skip? true)

    colon
    (ws \: ws :skip? true)

    sign
    [or \+ \-]

    quote
    [\" :skip? true]

    <obj
    [\{ :skip? true]

    obj>
    [\} :skip? true]

    <arr
    [\[ :skip? true]

    arr>
    [\] :skip? true]

    json/json
    (ws [or
         json/true
         json/false
         json/null
         json/number
         json/string
         json/array
         json/object] :coerce first)

    json/string
    (quote [* json/char :string? true] quote :coerce first)

    json/char
    [or
     [range [\u0020 \uffff] -\\ -\"]
     (\\ [or
          \"
          \\
          \/
          [\f :retunr \formfeed]
          [\b :return \backspace]
          [\r :return \return]
          [\n :return \newline]
          [\t :return \tab]
          (\u hex hex hex hex :coerce -parse-uXXXX)]
      :coerce second)]

    json/object
    [or
     (ws <obj ws obj> :return {})
     (ws <obj ws [join comma json/keyval :acc map] ws obj>
         :coerce first)]

    json/keyval
    (json/string colon json/json)

    json/digits+
    [+ json/digit :string? true]

    json/digits*
    [* json/digit :string? true]

    json/fraction
    (\. json/digits+ :string? true)

    json/exponent
    ([or \e \E] [? sign] json/digits+)

    json/number
    (json/integer [? (json/fraction [? json/exponent])] :coerce -parse-number)

    json/digit
    [range [\0 \9]]

    json/onenine
    [range [\1 \9]]

    json/integer
    ([? \- ]
     [or
      (json/onenine json/digits* :string? true)
      json/digit])

    json/array
    [or
     (<arr ws arr> :return [])
     (<arr ws [join comma json/json] ws arr> :coerce first)]

    json/true
    ["true" :return true]

    json/false
    ["false" :return false]

    json/null
    ["null" :return nil]})


(defn -parse-number [[[int-sign int-digits] [fraction [exp exp-sign exp-digits]]]]
  (if fraction
    (Double/parseDouble (str (or int-sign "")
                             int-digits
                             fraction
                             (if exp
                               (str exp (or exp-sign "") exp-digits)
                               "")))
    (Long/parseLong (str (or int-sign "") int-digits))))


(defn -parse-uXXXX [[_ h1 h2 h3 h4]]
  (char (Integer/parseInt (str h1 h2 h3 h4) 16)))


(def defs
  (o/compile grammar))


(defn parse-string [string]
  (o/parse defs 'json/json string))


(defn parse-stream [input & [{:keys [encoding]
                              :or {encoding "UTF-8"}}]]
  (o/parse defs
                'json/json
                (-> input
                    (io/reader :encoding encoding)
                    (o/reader->seq))))
