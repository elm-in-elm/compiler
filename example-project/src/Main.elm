module Main exposing (main)


main =
    List.map (\n -> n + 1)
        (List.map Tuple.first
            (List.sortBy Tuple.second
                (List.indexedMap Tuple.pair
                    (List.map Tuple.first
                        (List.sortBy Tuple.second
                            (List.indexedMap
                                Tuple.pair
                                [ 41, 17, 22, 35, 500 + 7 ]
                            )
                        )
                    )
                )
            )
        )
