module Set.Extra exposing (concatMap, filterMap, subset, toggle)

{-| Convenience functions for working with Set.

@docs concatMap, filterMap, subset, toggle

-}

import Set exposing (Set)


{-| Map a given function onto a set and union the resulting set.

    import Set exposing (Set)

    neighbors : (Int, Int) -> Set (Int, Int)
    neighbors (x, y) =
        Set.fromList
            [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
            , (x - 1, y),                 (x + 1, y)
            , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
            ]

    setOfPoints : Set (Int, Int)
    setOfPoints =
        Set.fromList [(1,1), (0,0)]

    Set.Extra.concatMap neighbors setOfPoints
    --> Set.fromList
    -->     [ (-1,-1), (-1,0), (-1,1)
    -->     , (0,-1), (0,0), (0,1)
    -->     , (0,2), (1,-1), (1,0)
    -->     , (1,1), (1,2), (2,0)
    -->     , (2,1), (2,2)
    -->     ]

-}
concatMap : (comparable -> Set comparable2) -> Set comparable -> Set comparable2
concatMap f s =
    Set.foldl (Set.union << f) Set.empty s


{-| Check if a Set is a subset of another Set.

    import Set exposing (Set)

    Set.Extra.subset
        (Set.fromList [1,2,3])
        (Set.fromList [1,2,3,4,5])
    --> True

-}
subset : Set comparable -> Set comparable -> Bool
subset s1 s2 =
    Set.size s1
        <= Set.size s2
        && Set.foldl (\x acc -> acc && Set.member x s2) True s1


{-| If the set does not contain the element, add it. If it does contain the element, remove it.

    import Set exposing (Set)

    Set.Extra.toggle 1 (Set.fromList [1,2,3])
    --> Set.fromList [2, 3]

    Set.Extra.toggle 1 (Set.fromList [2,3])
    --> Set.fromList [1, 2, 3]

-}
toggle : comparable -> Set comparable -> Set comparable
toggle elem set =
    if Set.member elem set then
        Set.remove elem set

    else
        Set.insert elem set


{-| Apply a function that may succeed to all values in the set, but only keep the successes.

    import Set exposing (Set)

    Set.fromList ["1", "2", "a", "3"]
        |> Set.Extra.filterMap String.toFloat
    --> Set.fromList [1, 2, 3]

-}
filterMap : (comparable -> Maybe comparable2) -> Set comparable -> Set comparable2
filterMap f xs =
    Set.fromList <| Set.foldr (maybeCons f) [] xs


maybeCons : (comparable -> Maybe comparable2) -> comparable -> List comparable2 -> List comparable2
maybeCons f mx xs =
    case f mx of
        Just x ->
            x :: xs

        Nothing ->
            xs
