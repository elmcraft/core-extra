module Maybe.Extra.AndMap exposing (..)


andMapOriginal : Maybe a -> Maybe (a -> b) -> Maybe b
andMapOriginal =
    Maybe.map2 (|>)


andMapInlined : Maybe a -> Maybe (a -> b) -> Maybe b
andMapInlined ra rb =
    case ( ra, rb ) of
        ( Just o, Just fn ) ->
            Just (fn o)

        ( _, Nothing ) ->
            Nothing

        ( Nothing, _ ) ->
            Nothing
