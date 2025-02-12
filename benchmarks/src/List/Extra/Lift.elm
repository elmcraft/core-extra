module List.Extra.Lift exposing (liftAndThen2, liftAndThen3, liftAndThen4, liftFold2, liftFold3, liftFold4)

{-| Map functions taking multiple arguments over multiple lists, regardless of list length.
All possible combinations will be explored.

    liftAndThen2 (+) [1,2,3][4,5]
    --> [5,6,6,7,7,8]

-}

import List.Extra exposing (andThen)


liftAndThen2 : (a -> b -> c) -> List a -> List b -> List c
liftAndThen2 f la lb =
    la |> andThen (\a -> lb |> andThen (\b -> [ f a b ]))


{-| Maps a function over three lists, exploring all possible combinations.
-}
liftAndThen3 : (a -> b -> c -> d) -> List a -> List b -> List c -> List d
liftAndThen3 f la lb lc =
    la |> andThen (\a -> lb |> andThen (\b -> lc |> andThen (\c -> [ f a b c ])))


{-| Maps a function over four lists, exploring all possible combinations.
-}
liftAndThen4 : (a -> b -> c -> d -> e) -> List a -> List b -> List c -> List d -> List e
liftAndThen4 f la lb lc ld =
    la |> andThen (\a -> lb |> andThen (\b -> lc |> andThen (\c -> ld |> andThen (\d -> [ f a b c d ]))))


{-| Map functions taking multiple arguments over multiple lists, regardless of list length.
All possible combinations will be explored.

    liftFold2 (+) [1,2,3][4,5]
    --> [5,6,6,7,7,8]

-}
liftFold2 : (a -> b -> c) -> List a -> List b -> List c
liftFold2 f la lb =
    List.foldl (\a aacc -> List.foldl (\b bacc -> f a b :: bacc) aacc lb) [] la
        |> List.reverse


{-| Maps a function over three lists, exploring all possible combinations.
-}
liftFold3 : (a -> b -> c -> d) -> List a -> List b -> List c -> List d
liftFold3 f la lb lc =
    List.foldl (\a aacc -> List.foldl (\b bacc -> List.foldl (\c cacc -> f a b c :: cacc) bacc lc) aacc lb) [] la
        |> List.reverse


{-| Maps a function over four lists, exploring all possible combinations.
-}
liftFold4 : (a -> b -> c -> d -> e) -> List a -> List b -> List c -> List d -> List e
liftFold4 f la lb lc ld =
    List.foldl (\a aacc -> List.foldl (\b bacc -> List.foldl (\c cacc -> List.foldl (\d dacc -> f a b c d :: dacc) cacc ld) bacc lc) aacc lb) [] la
        |> List.reverse
