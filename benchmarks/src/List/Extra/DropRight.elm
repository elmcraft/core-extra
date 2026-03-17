module List.Extra.DropRight exposing
    ( dropRightFoldr
    , dropRightLength
    , dropRightReverse
    )


dropRightFoldr : Int -> List a -> List a
dropRightFoldr n lst =
    let
        step x ( n_, acc ) =
            if n_ > 0 then
                ( n_ - 1, acc )

            else
                ( 0, x :: acc )
    in
    List.foldr step ( n, [] ) lst
        |> Tuple.second


dropRightReverse : Int -> List a -> List a
dropRightReverse n lst =
    lst
        |> List.reverse
        |> List.drop n
        |> List.reverse


dropRightLength : Int -> List a -> List a
dropRightLength n lst =
    lst |> List.take (List.length lst - n)
