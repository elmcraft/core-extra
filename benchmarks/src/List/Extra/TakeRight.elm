module List.Extra.TakeRight exposing
    ( takeRightFoldr
    , takeRightLength
    , takeRightReverse
    )


takeRightFoldr : Int -> List a -> List a
takeRightFoldr n lst =
    let
        step x ( n_, acc ) =
            if n_ > 0 then
                ( n_ - 1, x :: acc )

            else
                -- Wish we could stop here. List.Extra.stoppableFoldr where are you?
                ( 0, acc )
    in
    List.foldr step ( n, [] ) lst
        |> Tuple.second


takeRightReverse : Int -> List a -> List a
takeRightReverse n lst =
    lst
        |> List.reverse
        |> List.take n
        |> List.reverse


takeRightLength : Int -> List a -> List a
takeRightLength n lst =
    lst |> List.drop (List.length lst - n)
