module Result.Extra.AndMap exposing (andMapInlined, andMapInlinedNestedCaseOf, andMapOriginal)


andMapOriginal : Result e a -> Result e (a -> b) -> Result e b
andMapOriginal ra rb =
    case ( ra, rb ) of
        ( _, Err x ) ->
            Err x

        ( o, Ok fn ) ->
            Result.map fn o


andMapInlined : Result e a -> Result e (a -> b) -> Result e b
andMapInlined ra rb =
    case ( ra, rb ) of
        ( Ok o, Ok fn ) ->
            Ok (fn o)

        ( _, Err x ) ->
            Err x

        ( Err x, _ ) ->
            Err x


andMapInlinedNestedCaseOf : Result e a -> Result e (a -> b) -> Result e b
andMapInlinedNestedCaseOf ra rb =
    case ra of
        Ok o ->
            case rb of
                Ok fn ->
                    Ok (fn o)

                Err x ->
                    Err x

        Err x ->
            Err x
