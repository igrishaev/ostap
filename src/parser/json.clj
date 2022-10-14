(ns parser.json)

{   }

element
[or foo bar baz]

member
[= string \: element]

members
[= member [* [= \, member]]]

[\{ ws* [? members ] ws* \}]


(def member
  ...
  )

{"true" ...
 "false" ...


 }

'{

  json/string
  [\" json/characters \"]

  json/characters
  [* json/character]

  json/character
  [or
   [range [0x0020 0x10ffff] - \" \\ ]
   [or
    ["\\n" :coerce coerce/foo]
    ["\\r" :coerce coerce/foo]
    ["\\t" :coerce coerce/foo]]]

  json/elements
  {json/element {. {\, json/elements}}}

  json/elements
  [json/element
   [json/element \, json/elements]]

  json/array

  {:foo {. {:bar {. :baz}}}}


  [[foo]
   [foo bar]
   [foo bar baz]
   ]

  json/members
  [= json/member [* [= \, json/member]]]






  }





;; json/element
'[or json/object json/array json/string json/number "true" "false" "null"]


;; json/object
'[group \{ json/keywals \}]

;; json/keywals
'[joined \, json/keywal]

;; json/keywal

'[group json/string \: json/element]




'[or json/member [group json/member \, json/members]]

joined

members
  member
  member \, members


[joined \, alphanum]


(loop [phase true
       acc (acc-new)
       ]

  (let [parser
        (if phase item sep)]

    (let [result
          (parse parser source)]

      (if (failure? result)
        (failure ...)

        (if phase
          ()
          )

        )

      )





    )



  (recur (not phase))




  )



'[:fff "sdfsf" - "sdf" - "sdfsfd" "sdfsf"]

()
