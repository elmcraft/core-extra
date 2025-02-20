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


andMapSimplified : Maybe a -> Maybe (a -> b) -> Maybe b
andMapSimplified ra rb =
    case ( ra, rb ) of
        ( Just o, Just fn ) ->
            Just (fn o)

        _ ->
            Nothing

andMapNestedCaseOfIgnoringNothing : Maybe a -> Maybe (a -> b) -> Maybe b
andMapNestedCaseOfIgnoringNothing ra rb =
    case ra of
        Just o ->
            case rb of
                Just fn ->
                    Just (fn o)

                _ ->
                    Nothing

        _ ->
            Nothing

andMapNestedCaseOf : Maybe a -> Maybe (a -> b) -> Maybe b
andMapNestedCaseOf ra rb =
    case ra of
        Just o ->
            case rb of
                Just fn ->
                    Just (fn o)

                Nothing ->
                    Nothing

        Nothing ->
            Nothing
