



(compile {...})


(parse parser source)

(parse-main
 [parser source]
 [defs parser source])


(def gramma
  '{json
    [or json/array ...]

    json/array
    ...

    })


(def defs
  (compile gramma))


(with-defs defs
  (parse 'json "aaaaa"))
