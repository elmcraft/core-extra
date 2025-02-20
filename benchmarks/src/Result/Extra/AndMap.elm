module Result.Extra.AndMap exposing (andMapInlined, andMapOriginal)


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
