module Dict.Extra exposing
    ( groupBy, filterGroupBy, fromListBy, fromListDedupe, fromListDedupeBy, frequencies
    , removeWhen, removeMany, keepOnly, insertDedupe, mapKeys, filterMap, invert
    , any, find
    , unionWith, unionWithKey
    )

{-| Convenience functions for working with `Dict`


# List operations

@docs groupBy, filterGroupBy, fromListBy, fromListDedupe, fromListDedupeBy, frequencies


# Manipulation

@docs removeWhen, removeMany, keepOnly, insertDedupe, mapKeys, filterMap, invert


# Utilities

@docs any, find


# Combine

@docs unionWith, unionWithKey

-}

import Dict exposing (Dict)
import Set exposing (Set)


{-| Takes a key-fn and a list.
Creates a `Dict` which maps the key to a list of matching elements.

    import Dict

    groupBy String.length [ "tree" , "apple" , "leaf" ]
    --> Dict.fromList [ ( 4, [ "tree", "leaf" ] ), ( 5, [ "apple" ] ) ]

-}
groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy keyfn list =
    List.foldr
        (\x acc ->
            Dict.update (keyfn x) (Maybe.map ((::) x) >> Maybe.withDefault [ x ] >> Just) acc
        )
        Dict.empty
        list


{-| Takes a key-fn and a list.
Creates a `Dict` which maps the key to a list of matching elements, skipping elements
where key-fn returns `Nothing`

    import Dict

    filterGroupBy (String.uncons >> Maybe.map Tuple.first) [ "tree" , "", "tweet", "apple" , "leaf", "" ]
    --> Dict.fromList [ ( 't', [ "tree", "tweet" ] ), ( 'a', [ "apple" ] ), ( 'l', [ "leaf" ] ) ]

    filterGroupBy
        .car
        [ { name = "Mary"
          , car = Just "Ford"
          }
        , { name = "Jack"
          , car = Nothing
          }
        , { name = "Jill"
          , car = Just "Tesla"
          }
        , { name = "John"
          , car = Just "Tesla"
          }
        ]
    --> Dict.fromList
    --> [ ( "Ford"
    -->   , [ { name = "Mary" , car = Just "Ford" } ]
    -->   )
    --> , ( "Tesla"
    -->   , [ { name = "Jill" , car = Just "Tesla" }
    -->     , { name = "John" , car = Just "Tesla" }
    -->     ]
    -->   )
    --> ]

-}
filterGroupBy : (a -> Maybe comparable) -> List a -> Dict comparable (List a)
filterGroupBy keyfn list =
    List.foldr
        (\x acc ->
            case keyfn x of
                Just key ->
                    Dict.update key (Maybe.map ((::) x) >> Maybe.withDefault [ x ] >> Just) acc

                Nothing ->
                    acc
        )
        Dict.empty
        list


{-| Create a dictionary from a list of values, by passing a function that can get a key from any such value.
If the function does not return unique keys, earlier values are discarded.

    import Dict

    fromListBy String.length [ "tree" , "apple" , "leaf" ]
    --> Dict.fromList [ ( 4, "leaf" ), ( 5, "apple" ) ]

-}
fromListBy : (a -> comparable) -> List a -> Dict comparable a
fromListBy keyfn xs =
    List.foldl
        (\x acc -> Dict.insert (keyfn x) x acc)
        Dict.empty
        xs


{-| Like `Dict.fromList`, but you provide a way to deal with
duplicate keys. Create a dictionary from a list of pairs of keys and
values, providing a function that is used to combine multiple values
paired with the same key.

    import Dict

    fromListDedupe
        (\a b -> a ++ " " ++ b)
        [ ( "class", "menu" ), ( "width", "100%" ), ( "class", "big" ) ]
    --> Dict.fromList [ ( "class", "menu big" ), ( "width", "100%" ) ]

-}
fromListDedupe : (a -> a -> a) -> List ( comparable, a ) -> Dict comparable a
fromListDedupe combine xs =
    List.foldl
        (\( key, value ) acc -> insertDedupe combine key value acc)
        Dict.empty
        xs


{-| `fromListBy` and `fromListDedupe` rolled into one.

    import Dict

    fromListDedupeBy (\first second -> first) String.length [ "tree" , "apple" , "leaf" ]
    --> Dict.fromList [ ( 4, "tree" ), ( 5, "apple" ) ]

-}
fromListDedupeBy : (a -> a -> a) -> (a -> comparable) -> List a -> Dict comparable a
fromListDedupeBy combine keyfn xs =
    List.foldl
        (\x acc -> insertDedupe combine (keyfn x) x acc)
        Dict.empty
        xs


{-| Count the number of occurences for each of the elements in the list.

    import Dict

    frequencies [ "A", "B", "C", "B", "C", "B" ]
    --> Dict.fromList [ ( "A", 1 ), ( "B", 3 ), ( "C", 2 ) ]

-}
frequencies : List comparable -> Dict comparable Int
frequencies list =
    list
        |> List.foldl
            (\el counter ->
                Dict.get el counter
                    |> Maybe.withDefault 0
                    |> (\count -> count + 1)
                    |> (\count -> Dict.insert el count counter)
            )
            Dict.empty


{-| Remove elements which satisfies the predicate.

    import Dict

    Dict.fromList [ ( "Mary", 1 ), ( "Jack", 2 ), ( "Jill", 1 ) ]
        |> removeWhen (\_ value -> value == 1 )
    --> Dict.fromList [ ( "Jack", 2 ) ]

-}
removeWhen : (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
removeWhen pred dict =
    Dict.filter (\k v -> not (pred k v)) dict


{-| Remove a key-value pair if its key appears in the set.

    import Dict
    import Set

    Dict.fromList [ ( "Mary", 1 ), ( "Jack", 2 ), ( "Jill", 1 ) ]
        |> removeMany (Set.fromList [ "Mary", "Jill" ])
    --> Dict.fromList [ ( "Jack", 2 ) ]

-}
removeMany : Set comparable -> Dict comparable v -> Dict comparable v
removeMany set dict =
    Set.foldl Dict.remove dict set


{-| Insert an element at the given key, providing a combining
function that used in the case that there is already an
element at that key. The combining function is called with
original element and the new element as arguments and
returns the element to be inserted.

    import Dict

    Dict.fromList [ ( "expenses", 38.25 ), ( "assets", 100.85 ) ]
        |> insertDedupe (+) "expenses" 2.50
        |> insertDedupe (+) "liabilities" -2.50
    --> Dict.fromList [ ( "expenses", 40.75 ), ( "assets", 100.85 ), ( "liabilities", -2.50 ) ]

-}
insertDedupe : (v -> v -> v) -> comparable -> v -> Dict comparable v -> Dict comparable v
insertDedupe combine key value dict =
    let
        with mbValue =
            case mbValue of
                Just oldValue ->
                    Just <| combine oldValue value

                Nothing ->
                    Just value
    in
    Dict.update key with dict


{-| Keep a key-value pair if its key appears in the set.

    import Dict
    import Set

    Dict.fromList [ ( "Mary", 1 ), ( "Jack", 2 ), ( "Jill", 1 ) ]
        |> keepOnly (Set.fromList [ "Jack", "Jill" ])
    --> Dict.fromList [ ( "Jack", 2 ), ( "Jill", 1 ) ]

-}
keepOnly : Set comparable -> Dict comparable v -> Dict comparable v
keepOnly set dict =
    Set.foldl
        (\k acc ->
            Maybe.withDefault acc <| Maybe.map (\v -> Dict.insert k v acc) (Dict.get k dict)
        )
        Dict.empty
        set


{-| Apply a function to all keys in a dictionary.

    import Dict

    Dict.fromList [ ( 5, "Jack" ), ( 10, "Jill" ) ]
        |> mapKeys (\x -> x + 1)
    --> Dict.fromList [ ( 6, "Jack" ), ( 11, "Jill" ) ]

    Dict.fromList [ ( 5, "Jack" ), ( 10, "Jill" ) ]
        |> mapKeys String.fromInt
    --> Dict.fromList [ ( "5", "Jack" ), ( "10", "Jill" ) ]

-}
mapKeys : (comparable -> comparable1) -> Dict comparable v -> Dict comparable1 v
mapKeys keyMapper dict =
    Dict.foldl
        (\k v acc ->
            Dict.insert (keyMapper k) v acc
        )
        Dict.empty
        dict


{-| Apply a function that may or may not succeed to all entries in a dictionary,
but only keep the successes.

    import Dict

    let
        isTeen n a =
            if 13 <= n && n <= 19 then
                Just <| String.toUpper a
            else
                Nothing
    in
    Dict.fromList [ ( 5, "Jack" ), ( 15, "Jill" ), ( 20, "Jones" ) ]
        |> filterMap isTeen
    --> Dict.fromList [ ( 15, "JILL" ) ]

-}
filterMap : (comparable -> a -> Maybe b) -> Dict comparable a -> Dict comparable b
filterMap f dict =
    Dict.foldl
        (\k v acc ->
            case f k v of
                Just newVal ->
                    Dict.insert k newVal acc

                Nothing ->
                    acc
        )
        Dict.empty
        dict


{-| Inverts the keys and values of an array.

    import Dict

    Dict.fromList [ ("key", "value")  ]
        |> invert
    --> Dict.fromList [ ( "value", "key" ) ]

-}
invert : Dict comparable1 comparable2 -> Dict comparable2 comparable1
invert dict =
    Dict.foldl
        (\k v acc ->
            Dict.insert v k acc
        )
        Dict.empty
        dict


{-| Determine if any key/value pair satisfies some test.

    import Dict

    Dict.fromList [ ( 9, "Jill" ), ( 7, "Jill" ) ]
        |> any (\_ value -> value == "Jill")
    --> True

    Dict.fromList [ ( 9, "Jill" ), ( 7, "Jill" ) ]
        |> any (\key _ -> key == 5)
    --> False

-}
any : (comparable -> a -> Bool) -> Dict comparable a -> Bool
any predicate dict =
    Dict.foldl
        (\k v acc ->
            if acc then
                acc

            else
                predicate k v
        )
        False
        dict


{-| Find the first key/value pair that matches a predicate.

    import Dict

    Dict.fromList [ ( 9, "Jill" ), ( 7, "Jill" ) ]
        |> find (\_ value -> value == "Jill")
    --> Just ( 7, "Jill" )

    Dict.fromList [ ( 9, "Jill" ), ( 7, "Jill" ) ]
        |> find (\key _ -> key == 5)
    --> Nothing

-}
find : (comparable -> a -> Bool) -> Dict comparable a -> Maybe ( comparable, a )
find predicate dict =
    Dict.foldl
        (\k v acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    if predicate k v then
                        Just ( k, v )

                    else
                        Nothing
        )
        Nothing
        dict


{-| Combine two dictionaries. If there is a collision, a combining function is
used to combine the two values.

    import Dict

    unionWith (+)
        (Dict.fromList [ ( "a", 2 ), ( "b", 3 ) ])
        (Dict.fromList [ ( "b", 4 ), ( "c", 1 ) ])
        --> Dict.fromList [ ( "a", 2 ), ( "b", 7 ), ( "c", 1 ) ]

    unionWith (++)
        (Dict.fromList [ ( 1, "123" ), ( 2, "abc" ) ])
        (Dict.fromList [ ( 2, "def" ), ( 3, "xyz" ) ])
        --> Dict.fromList [ ( 1, "123" ), ( 2, "abcdef" ), ( 3, "xyz" ) ]

Note that, like `Dict.union`, it is more efficient to have the larger `Dict` as
the second argument, i.e. when possible, you should use `unionWith f new old`,
if `old` has more keys than `new`.

-}
unionWith : (a -> a -> a) -> Dict comparable a -> Dict comparable a -> Dict comparable a
unionWith f =
    unionWithKey (\_ -> f)


{-| Combine two dictionaries. If there is a collision, a combining function is
used to combine the two values. Unlike `unionWith`, the combining function also
has access to the key.

    import Dict

    unionWithKey (\k v1 v2 -> String.fromInt k ++ v1 ++ v2 )
        (Dict.fromList [ ( 1, "123" ), ( 2, "abc" ) ])
        (Dict.fromList [ ( 2, "def" ), ( 3, "xyz" ) ])
        --> Dict.fromList [ ( 1, "123" ), ( 2, "2abcdef" ), ( 3, "xyz" ) ]

Note that, like `Dict.union`, it is more efficient to have the larger `Dict` as
the second argument, i.e. when possible, you should use `unionWith f new old`,
if `old` has more keys than `new`.

-}
unionWithKey : (comparable -> a -> a -> a) -> Dict comparable a -> Dict comparable a -> Dict comparable a
unionWithKey f d1 d2 =
    Dict.foldl
        (\k v1 acc ->
            case Dict.get k acc of
                Just v2 ->
                    Dict.insert k (f k v1 v2) acc

                Nothing ->
                    Dict.insert k v1 acc
        )
        d2
        d1
