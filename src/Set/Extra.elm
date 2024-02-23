module Set.Extra exposing
    ( toggle
    , isSupersetOf, isSubsetOf, areDisjoint
    , symmetricDifference
    , concatMap, filterMap
    , subset
    )

{-| Convenience functions for working with Set.


# Toggling elements

@docs toggle


# Predicates

@docs isSupersetOf, isSubsetOf, areDisjoint


# Set operations

@docs symmetricDifference


# Mapping

@docs concatMap, filterMap


# Deprecated functions

These functions are deprecated and **will be removed** in the next major version of this library.

@docs subset

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


{-| A set is a subset of another set if all the elements in the first set appear in the second set.

    import Set exposing (Set)

    Set.fromList [ 1, 2, 3 ]
        |> Set.Extra.isSubsetOf (Set.fromList [1,2,3,4,5])
    --> True

-}
isSubsetOf : Set comparable -> Set comparable -> Bool
isSubsetOf s2 s1 =
    Set.size s1
        <= Set.size s2
        && Set.foldl (\x acc -> acc && Set.member x s2) True s1


{-| A set is a superset of another set if all the elements in the second set appear in the first set.

    import Set exposing (Set)


    Set.fromList [ 1, 2, 3 ]
        |> Set.Extra.isSupersetOf (Set.fromList [1,2,3,4,5])
    --> False

Note: This is just isSubsetOf with arguments reversed. It can be handy for dealing with pipelines.

-}
isSupersetOf : Set comparable -> Set comparable -> Bool
isSupersetOf s1 s2 =
    isSubsetOf s2 s1


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


{-| A set is disjoint from another set if they have no elements in common.

    import Set exposing (Set)

    Set.Extra.areDisjoint
        (Set.fromList [1,2,3])
        (Set.fromList [3,4,5])
    --> False

    Set.Extra.areDisjoint
        (Set.fromList [1,2,3])
        (Set.fromList [4,5,6])
    --> True

-}
areDisjoint : Set comparable -> Set comparable -> Bool
areDisjoint a b =
    not (Set.foldl (\x so -> so || Set.member x b) False a)


{-| Check if a Set is a subset of another Set.

    import Set exposing (Set)

    Set.Extra.subset
        (Set.fromList [1,2,3])
        (Set.fromList [1,2,3,4,5])
    --> True

@deprecated in favour of isSubsetOf

-}
subset : Set comparable -> Set comparable -> Bool
subset =
    isSupersetOf


{-| The symmetric difference between two sets is a set that contains all the elements that are in one of the two sets, but not both.

    import Set exposing (Set)


    Set.Extra.symmetricDifference
        (Set.fromList [1,2,3])
        (Set.fromList [3,4,5])
        --> Set.fromList [1,2,4,5]

-}
symmetricDifference : Set comparable -> Set comparable -> Set comparable
symmetricDifference a b =
    symmetricDifferenceHelp (Set.toList a) (Set.toList b) Set.empty


symmetricDifferenceHelp : List comparable -> List comparable -> Set comparable -> Set comparable
symmetricDifferenceHelp a b soFar =
    case ( a, b ) of
        ( x :: xs, y :: ys ) ->
            case compare x y of
                EQ ->
                    symmetricDifferenceHelp xs ys soFar

                GT ->
                    symmetricDifferenceHelp a ys (Set.insert y soFar)

                LT ->
                    symmetricDifferenceHelp xs b (Set.insert x soFar)

        _ ->
            Set.union (Set.union (Set.fromList a) (Set.fromList b)) soFar
