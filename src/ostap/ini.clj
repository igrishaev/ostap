(ns ostap.ini
  (:require
   [clojure.java.io :as io]
   [ostap.core :as o]))


(def grammar
  '
  {
   br
   [or "\r\n" "\n" :skip? true]

   ws
   [* [or \space \tab] :skip? true]

   line/comment
   (ws \# [+ [range [\u0020 \uffff] -\newline -\return]] :skip? true)

   line/blank
   [* [range \space \tab] :skip? true]

   <title
   [\[ :skip? true]

   title>
   [\] :skip? true]

   separator
   (br [* [range \space \tab \newline \return \tab]] :skip? true)

   ini/headline
   (ws <title ws ini/title ws title> ws :coerce first)

   ini/title
   [+ [range [\u0020 \uffff] -\]] :string? true]

   ini/key
   [+ [range [\u0020 \uffff] -\= -\space] :string? true]

   ini/val
   [or
    (\' [* [range [\u0020 \uffff] -\'] :string? true] \' :coerce second)
    (\" [* [range [\u0020 \uffff] -\"] :string? true] \" :coerce second)
    [* [range [\u0020 \uffff] -\space] :string? true]]

   ini/keyval
   (ws ini/key (ws \= ws :skip? true) ini/val ws)

   ini/keyvals
   [join separator ini/keyval]

   digits
   [or
    (digit [\, :skip? true] digits)
    digit]

   digit
   [range [\0 \9]]

   ;; section
   ;;   title
   ;;   title br
   ;;   title br body

   ;; line
   ;;   comment
   ;;   blank
   ;;   keyval

   ;; body
   ;;   line
   ;;   line br body


   ;; section
   ;; section br body

   ;; line
   ;;   blank
   ;;   comment
   ;;   keywal

   ;; body
   ;;   [line br line]
   ;;   line
   ;;   nil



   ini/section
   (ini/headline
    [or
     (br [join br [or ini/keyval line/comment line/blank]])
     ini/headline])

   ini/ini
   [* ini/section]







   }

  )

(def defs
  (o/compile grammar))


(defn parse-string [string]
  (o/parse defs 'ini/headline string))


(def INI
  "   [ hello ]   ")
