module Array.Extra exposing
    ( all, any, member
    , reverse, intersperse, update, pop, removeAt, insertAt
    , removeWhen, filterMap
    , sliceFrom, sliceUntil, splitAt, unzip, last
    , interweave_, andMap, map2, map3, map4, map5, zip, zip3
    , resizelRepeat, resizerRepeat, resizelIndexed, resizerIndexed
    , mapToList, indexedMapToList
    )

{-| Convenience functions for working with `Array`


# Predicates

@docs all, any, member


# Alter

@docs reverse, intersperse, update, pop, removeAt, insertAt


# Filtering

@docs removeWhen, filterMap


# Getting slices of an array

@docs sliceFrom, sliceUntil, splitAt, unzip, last


# Combining arrays

@docs interweave_, andMap, map2, map3, map4, map5, zip, zip3


# Resizing

@docs resizelRepeat, resizerRepeat, resizelIndexed, resizerIndexed


# To List

@docs mapToList, indexedMapToList

-}

import Array exposing (Array, append, empty, initialize, length, repeat, slice)


{-| Update the element at a given index based on its current value.
If the index is out of bounds, nothing is changed.

    import Array exposing (fromList)

    fromList [ 1, 2, 3 ] |> update 1 (\n -> n + 10)
    --> fromList [ 1, 12, 3 ]

    fromList [ 1, 2, 3 ] |> update 4 (\n -> n + 10)
    --> fromList [ 1, 2, 3 ]

    fromList [ 1, 2, 3 ] |> update -1 (\n -> n + 10)
    --> fromList [ 1, 2, 3 ]

-}
update : Int -> (a -> a) -> Array a -> Array a
update index alter array =
    case Array.get index array of
        Nothing ->
            array

        Just element ->
            Array.set index (alter element) array


{-| Drop a given number of elements from the start.
In other words, slice the `Array` from an index until the very end.
Given a negative argument, count the end of the slice from the end.

    import Array exposing (fromList)

    fromList (List.range 0 6) |> sliceFrom 3
    --> fromList [ 3, 4, 5, 6 ]

    fromList (List.range 0 6) |> sliceFrom -3
    --> fromList [ 4, 5, 6 ]

-}
sliceFrom : Int -> Array a -> Array a
sliceFrom lengthDropped array =
    slice lengthDropped (length array) array


{-| Take a number of elements from the start.
In other words, slice the `Array` from the very beginning until not including the index.
Given a negative argument, count the beginning of the slice from the end.

    import Array exposing (fromList)

    fromList (List.range 0 6) |> sliceUntil 3
    --> fromList [ 0, 1, 2 ]

    fromList (List.range 0 6) |> sliceUntil -3
    --> fromList [ 0, 1, 2, 3 ]

-}
sliceUntil : Int -> Array a -> Array a
sliceUntil exclusiveEndIndex array =
    slice 0 exclusiveEndIndex array


{-| Remove the last element.

    import Array exposing (fromList, empty)

    fromList [ 1, 2, 3 ] |> pop
    --> fromList [ 1, 2 ]

    empty |> pop
    --> empty

-}
pop : Array a -> Array a
pop array =
    slice 0 -1 array


{-| Place a value between all elements.

    import Array exposing (fromList)

    fromList [ "turtles", "turtles", "turtles" ]
        |> intersperse "on"
    --> fromList
    -->     [ "turtles", "on", "turtles", "on", "turtles" ]

To interlace an `Array`, [`interweave_`](#interweave_).

-}
intersperse : a -> Array a -> Array a
intersperse separator array =
    array
        |> Array.toList
        |> List.intersperse separator
        |> Array.fromList


{-| Try transforming all elements but only keep the successes.

    import Array exposing (fromList)

    fromList [ "3", "4.0", "5", "hats" ]
        |> filterMap String.toInt
    --> fromList [ 3, 5 ]

-}
filterMap : (a -> Maybe b) -> Array a -> Array b
filterMap tryMap array =
    array
        |> Array.foldr
            (\el soFar -> consTry (tryMap el) soFar)
            []
        |> Array.fromList


consTry : Maybe a -> List a -> List a
consTry maybeNewHead list =
    case maybeNewHead of
        Just newHead ->
            newHead :: list

        Nothing ->
            list


{-| Map functions taking multiple arguments over multiple arrays. Each array should be of the same length; extra elements are dropped.

    import Array exposing (Array)

    toIntFunctions : Array (Float -> Int)
    toIntFunctions =
        Array.fromList
            [ round
            , floor
            , ceiling
            , truncate
            ]

    toIntFunctions
        |> andMap (Array.fromList [ -1.5, -1.5, -1.5, -1.5 ])
        --> Array.fromList [ -1, -2, -1, -1 ]

-}
andMap : Array a -> Array (a -> b) -> Array b
andMap x f =
    map2 (|>) x f


{-| Apply a function to the elements in the array and collect the result in a List.

    import Array exposing (fromList)
    import Html

    fromList [ "a", "b", "c" ]
        |> mapToList Html.text
    --> [ Html.text "a", Html.text "b", Html.text "c" ]

-}
mapToList : (a -> b) -> Array a -> List b
mapToList fn array =
    Array.foldr (\element soFar -> fn element :: soFar) [] array


{-| Transform all elements with their indexes as the first argument
and collect the result in a `List`.

    import Array exposing (Array, fromList)
    import Html exposing (Html)

    type alias Exercise =
        { name : String }

    exerciseRender : Int -> Exercise -> Html msg
    exerciseRender index =
        \exercise ->
            String.concat
                [ "Exercise #"
                , String.fromInt (index + 1)
                , " - "
                , exercise.name
                ]
                |> Html.text

    exercisesRender : Array Exercise -> Html msg
    exercisesRender =
        indexedMapToList renderExercise
            >> Html.div []

-}
indexedMapToList : (Int -> a -> b) -> Array a -> List b
indexedMapToList fn array =
    array
        |> Array.foldr
            (\element ( i, listSoFar ) ->
                ( i - 1
                , fn i element :: listSoFar
                )
            )
            ( length array - 1, [] )
        |> Tuple.second


{-| Combine the elements of two `Array`s with a given function.
If one `Array` is longer, its extra elements are not used.

    import Array exposing (fromList)

    map2 (\a b -> a + b)
        (fromList [ 1, 2, 3 ])
        (fromList [ 1, 2, 3, 4 ])
    --> fromList [ 2, 4, 6 ]

    map2 Tuple.pair
        (fromList [ 1, 2, 3 ])
        (fromList [ 'a', 'b' ])
    --> fromList [ ( 1, 'a' ), ( 2, 'b' ) ]

Note: [`zip`](Array-Extra#zip) can be used instead of `map2 Tuple.pair`.

-}
map2 :
    (a -> b -> combined)
    -> Array a
    -> Array b
    -> Array combined
map2 elementsCombine aArray bArray =
    List.map2 elementsCombine
        (Array.toList aArray)
        (Array.toList bArray)
        |> Array.fromList


{-| Combine the elements of three `Array`s with the given function. See [`map2`](Array-Extra#map2).

Note: [`zip3`](Array-Extra#zip3) can be used instead of `map3 (\a b c -> ( a, b, c ))`.

-}
map3 :
    (a -> b -> c -> combined)
    -> Array a
    -> Array b
    -> Array c
    -> Array combined
map3 elementsCombine aArray bArray cArray =
    List.map3 elementsCombine
        (Array.toList aArray)
        (Array.toList bArray)
        (Array.toList cArray)
        |> Array.fromList


{-| Combine the elements of four `Array`s with the given function. See [`map2`](Array-Extra#map2).
-}
map4 :
    (a -> b -> c -> d -> combined)
    -> Array a
    -> Array b
    -> Array c
    -> Array d
    -> Array combined
map4 elementsCombine aArray bArray cArray dArray =
    List.map4 elementsCombine
        (Array.toList aArray)
        (Array.toList bArray)
        (Array.toList cArray)
        (Array.toList dArray)
        |> Array.fromList


{-| Combine the elements of five `Array`s with the given function. See [`map2`](Array-Extra#map2).
-}
map5 :
    (a -> b -> c -> d -> e -> combined)
    -> Array a
    -> Array b
    -> Array c
    -> Array d
    -> Array e
    -> Array combined
map5 elementsCombine aArray bArray cArray dArray eArray =
    List.map5 elementsCombine
        (Array.toList aArray)
        (Array.toList bArray)
        (Array.toList cArray)
        (Array.toList dArray)
        (Array.toList eArray)
        |> Array.fromList


{-| Combine the elements of two `Array`s into tuples.
If one is longer, its extra elements are not used.

    import Array exposing (fromList)

    zip
        (fromList [ 1, 2, 3 ])
        (fromList [ 'a', 'b' ])
    --> fromList [ ( 1, 'a' ), ( 2, 'b' ) ]

-}
zip : Array a -> Array b -> Array ( a, b )
zip firstArray secondArray =
    map2 Tuple.pair firstArray secondArray


{-| Zip the elements of three `Array`s into 3-tuples.
Only the indexes of the shortest `Array` are used.

    import Array exposing (fromList)

    zip3
        (fromList [ 1, 2, 3 ])
        (fromList [ 'a', 'b' ])
        (fromList [ "a", "b", "c", "d" ])
    --> fromList
    -->     [ ( 1, 'a', "a" )
    -->     , ( 2, 'b', "b" )
    -->     ]

-}
zip3 : Array a -> Array b -> Array c -> Array ( a, b, c )
zip3 firstArray secondArray thirdArray =
    map3 (\a b c -> ( a, b, c )) firstArray secondArray thirdArray


{-| Split all tuple elements into a tuple of one `Array` with the first and one with the second values.

    import Array exposing (fromList)

    unzip
        (fromList
            [ ( 1, 'a' ), ( 2, 'b' ), ( 3, 'c' ) ]
        )
    --> ( fromList [ 1, 2, 3 ]
    --> , fromList [ 'a', 'b', 'c' ]
    --> )

-}
unzip : Array ( a, b ) -> ( Array a, Array b )
unzip arrayOfTuple =
    ( Array.map Tuple.first arrayOfTuple
    , Array.map Tuple.second arrayOfTuple
    )


{-| Get the last element in an `Array` if it exists.

    import Array exposing (fromList)

    last (fromList [ 'a', 'b', 'c' ])
    --> Just 'c'

    last (fromList [])
    --> Nothing

-}
last : Array a -> Maybe a
last array =
    Array.get (Array.length array - 1) array


{-| Only keep elements which fail to satisfy a given predicate.
This is equivalent to `Array.filter (not << predicate)`.

    import Array exposing (fromList)

    fromList [ -1, 92, 0, 14, -3 ]
        |> removeWhen (\x -> x < 0)
    --> fromList [ 92, 0, 14 ]

-}
removeWhen : (a -> Bool) -> Array a -> Array a
removeWhen shouldRemove array =
    Array.filter (\element -> not (shouldRemove element)) array


{-| Resize from the left, padding the right-hand side with a given value.

    import Array exposing (fromList, empty)

    fromList [ 1, 2 ] |> resizelRepeat 4 0
    --> fromList [ 1, 2, 0, 0 ]

    fromList [ 1, 2, 3 ] |> resizelRepeat 2 0
    --> fromList [ 1, 2 ]

    fromList [ 1, 2 ] |> resizelRepeat -1 0
    --> empty

-}
resizelRepeat : Int -> a -> Array a -> Array a
resizelRepeat lengthNew padding array =
    if lengthNew <= 0 then
        Array.empty

    else
        let
            arrayLength : Int
            arrayLength =
                length array
        in
        case compare arrayLength lengthNew of
            EQ ->
                array

            GT ->
                sliceUntil lengthNew array

            LT ->
                append array (repeat (lengthNew - arrayLength) padding)


{-| Resize from the right, padding the left-hand side with a given value.

    import Array exposing (fromList, empty)

    fromList [ 1, 2 ] |> resizerRepeat 4 0
    --> fromList [ 0, 0, 1, 2 ]

    fromList [ 1, 2, 3 ] |> resizerRepeat 2 0
    --> fromList [ 2, 3 ]

    fromList [ 1, 2 ] |> resizerRepeat -1 0
    --> empty

-}
resizerRepeat : Int -> a -> Array a -> Array a
resizerRepeat lengthNew defaultValue array =
    let
        arrayLength : Int
        arrayLength =
            length array
    in
    case compare arrayLength lengthNew of
        EQ ->
            array

        GT ->
            slice (arrayLength - lengthNew) arrayLength array

        LT ->
            append
                (repeat (lengthNew - arrayLength) defaultValue)
                array


{-| Resize from the left, padding the right-hand side with a given value based on index.

    import Array exposing (fromList, empty)

    fromList [ 'a', 'b', 'c' ]
        |> resizelIndexed 5 toLetterInAlphabet
    --> fromList [ 'a', 'b', 'c', 'd', 'e' ]

    fromList [ 'a', 'b', 'c' ]
        |> resizelIndexed 2 toLetterInAlphabet
    --> fromList [ 'a', 'b' ]

    fromList [ 'a', 'b', 'c' ]
        |> resizelIndexed -1 toLetterInAlphabet
    --> empty

    toLetterInAlphabet : Int -> Char
    toLetterInAlphabet inAlphabet =
        Char.fromCode ((Char.toCode 'a') + inAlphabet)

-}
resizelIndexed : Int -> (Int -> a) -> Array a -> Array a
resizelIndexed lengthNew paddingElementForIndex array =
    if lengthNew <= 0 then
        Array.empty

    else
        let
            arrayLength : Int
            arrayLength =
                length array
        in
        case compare arrayLength lengthNew of
            EQ ->
                array

            GT ->
                sliceUntil lengthNew array

            LT ->
                append array
                    (initialize (lengthNew - arrayLength)
                        (\padIndex ->
                            paddingElementForIndex (arrayLength + padIndex)
                        )
                    )


{-| Resize from the right, padding the left-hand side with a given value based on index.

    import Array exposing (fromList, empty)

    fromList [ 10, 25, 36 ]
        |> resizerIndexed 5 (\n -> n * 5)
    --> fromList [ 0, 5, 10, 25, 36 ]

    fromList [ 10, 25, 36 ]
        |> resizerIndexed 2 (\n -> n * 5)
    --> fromList [ 25, 36 ]

    fromList [ 10, 25, 36 ]
        |> resizerIndexed -1 (\n -> n * 5)
    --> empty

-}
resizerIndexed : Int -> (Int -> a) -> Array a -> Array a
resizerIndexed lengthNew paddingAtIndex array =
    let
        arrayLength : Int
        arrayLength =
            length array
    in
    case compare arrayLength lengthNew of
        EQ ->
            array

        GT ->
            slice (arrayLength - lengthNew) arrayLength array

        LT ->
            append
                (initialize (lengthNew - arrayLength) paddingAtIndex)
                array


{-| Flip the element order.

    import Array exposing (fromList)

    fromList [ 1, 2, 3, 4 ] |> reverse
    --> fromList [ 4, 3, 2, 1 ]

-}
reverse : Array a -> Array a
reverse array =
    array
        |> reverseToList
        |> Array.fromList


reverseToList : Array a -> List a
reverseToList array =
    Array.foldl (::) [] array


{-| Split into two `Array`s, the first ending before and the second starting with a given index.

    import Array exposing (fromList, empty)

    fromList [ 1, 2, 3, 4 ] |> splitAt 2
    --> ( fromList [ 1, 2 ], fromList [ 3, 4 ] )

    fromList [ 1, 2, 3, 4 ] |> splitAt 100
    --> ( fromList [ 1, 2, 3, 4 ], empty )

    fromList [ 1, 2, 3, 4 ] |> splitAt -1
    --> ( empty, fromList [ 1, 2, 3, 4 ] )

-}
splitAt : Int -> Array a -> ( Array a, Array a )
splitAt index array =
    if index <= 0 then
        ( empty, array )

    else
        -- index >= 1
        ( sliceUntil index array
        , sliceFrom index array
        )


{-| Remove the element at a given index.
If the index is out of bounds, nothing is changed.

    import Array exposing (fromList)

    fromList [ 1, 2, 3, 4 ] |> removeAt 2
    --> fromList [ 1, 2, 4 ]

    fromList [ 1, 2, 3, 4 ] |> removeAt -1
    --> fromList [ 1, 2, 3, 4 ]

    fromList [ 1, 2, 3, 4 ] |> removeAt 100
    --> fromList [ 1, 2, 3, 4 ]

-}
removeAt : Int -> Array a -> Array a
removeAt index array =
    if index <= -1 then
        array

    else
        -- index >= 0
        let
            ( beforeIndex, startingAtIndex ) =
                splitAt index array

            lengthStartingAtIndex : Int
            lengthStartingAtIndex =
                length startingAtIndex
        in
        if lengthStartingAtIndex == 0 then
            beforeIndex

        else
            append beforeIndex
                (slice 1 lengthStartingAtIndex startingAtIndex)


{-| Insert an element at a given index.
If the index is out of bounds, nothing is changed.

    import Array exposing (fromList)

    fromList [ 'a', 'c' ] |> insertAt 1 'b'
    --> fromList [ 'a', 'b', 'c' ]

    fromList [ 'a', 'c' ] |> insertAt -1 'b'
    --> fromList [ 'a', 'c' ]

    fromList [ 'a', 'c' ] |>  insertAt 100 'b'
    --> fromList [ 'a', 'c' ]

-}
insertAt : Int -> a -> Array a -> Array a
insertAt index elementToInsert array =
    if index <= -1 then
        array

    else
        -- index >= 0
        let
            arrayLength =
                length array
        in
        if index <= arrayLength then
            let
                before =
                    Array.slice 0 index array

                after =
                    Array.slice index arrayLength array
            in
            Array.append (before |> Array.push elementToInsert) after

        else
            array


{-| Whether all elements satisfy a given test.

    import Array exposing (fromList, empty)

    fromList [ 2, 4 ] |> all (\x -> x < 5)
    --> True

    fromList [ 4, 16 ] |> all (\x -> x < 5)
    --> False

    empty |> all (\x -> x < 5)
    --> True

-}
all : (a -> Bool) -> Array a -> Bool
all isOkay array =
    Array.foldl
        (\element soFar -> soFar && isOkay element)
        True
        array


{-| Whether at least some elements satisfy a given test.

    import Array exposing (fromList, empty)

    fromList [ 6, 3 ] |> any (\x -> x < 5)
    --> True

    fromList [ 12, 33 ] |> any (\x -> x < 5)
    --> False

    empty |> any (\x -> x < 5)
    --> False

-}
any : (a -> Bool) -> Array a -> Bool
any isOkay array =
    Array.foldl
        (\element soFar -> soFar || isOkay element)
        False
        array


{-| Whether a given value is contained.

    import Array exposing (fromList)

    fromList [ "Leonardo", "Michelangelo", "Donatello", "Raphael" ]
        |> member "Donatello"
    --> True

    fromList [ "Leonardo", "Michelangelo" ]
        |> member "Raphael"
    --> False

For checking if some aspect is present, use [`any`](#any).

-}
member : a -> Array a -> Bool
member needle array =
    any (\element -> element == needle) array


{-| Return an array that contains elements from the two provided, in alternate order.
If one array runs out of items, append the items from the remaining array.

    import Array exposing (fromList, repeat)

    interweave_ (fromList [ "turtles", "turtles", "turtles" ]) (repeat 2 "on")
    --> fromList [ "turtles", "on", "turtles", "on", "turtles" ]

    interweave_ (fromList [ "turtles", "turtles", "turtles" ]) (repeat 5 "on")
    --> fromList [ "turtles", "on", "turtles", "on", "turtles", "on", "on", "on" ]

    interweave_ (fromList [ "turtles", "turtles", "turtles" ]) (repeat 1 "on")
    --> fromList [ "turtles", "on", "turtles", "turtles" ]

-}
interweave_ : Array a -> Array a -> Array a
interweave_ array toInterweave =
    let
        untilArrayEnd : { toInterweave : List a, interwoven : List a }
        untilArrayEnd =
            Array.foldl
                (\element soFar ->
                    case soFar.toInterweave of
                        [] ->
                            { interwoven =
                                element :: soFar.interwoven
                            , toInterweave = []
                            }

                        toInterweaveHead :: toInterweaveTail ->
                            { interwoven =
                                toInterweaveHead :: element :: soFar.interwoven
                            , toInterweave = toInterweaveTail
                            }
                )
                { interwoven = []
                , toInterweave = Array.toList toInterweave
                }
                array
    in
    List.reverse untilArrayEnd.interwoven
        ++ untilArrayEnd.toInterweave
        |> Array.fromList
