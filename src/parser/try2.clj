(ns parser.try2)


(defprotocol IParser

  (parse [this chars])

  )


(defrecord StringParser [string]

  IParser

  (parse [_ chars]
    ))


(defrecord GroupParser [parsers]

  IParser

  (parse [_ chars]
    (loop [i 0
           acc []
           chars chars
           parsers parsers]
      (if-let [parser (first parsers)]
        (let [{:keys [success? data chars error?]}
              (parse parser chars)]
          (cond
            success?
            (recur (inc i) (conj acc data) chars (next parsers))
            error?
            (failure "aaaa" (conj acc result))))
        (success acc chars)))))


(defrecord ?Parser [parser]

  IParser

  (parse [_ chars]
    (let [{:as result :keys [success? error?]}
          (parse parser chars)]
      (cond
        success?
        (success data chars)


        )
      )

    )
  )
