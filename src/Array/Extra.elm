module Array.Extra exposing
    ( all, any, member
    , reverse, intersperse
    , update, pop, removeAt, insertAt
    , removeWhen, filterMap
    , sliceFrom, sliceUntil, splitAt, unzip
    , interweave, apply, map2, map3, map4, map5, zip, zip3
    , resizelRepeat, resizerRepeat, resizelIndexed, resizerIndexed
    , mapToList, indexedMapToList
    )

{-| Convenience functions for working with `Array`


# observe

@docs all, any, member


# alter

@docs reverse, intersperse
@docs update, pop, removeAt, insertAt


## filter

@docs removeWhen, filterMap


## part

@docs sliceFrom, sliceUntil, splitAt, unzip


## combine

@docs interweave, apply, map2, map3, map4, map5, zip, zip3


## resize

@docs resizelRepeat, resizerRepeat, resizelIndexed, resizerIndexed


# transform

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
update :
    Int
    -> (element -> element)
    -> (Array element -> Array element)
update index alter =
    \array ->
        case array |> Array.get index of
            Nothing ->
                array

            Just element ->
                array |> Array.set index (alter element)


{-| Drop a given number of elements from the start.
In other words, slice the `Array` from an index until the very end.
Given a negative argument, count the end of the slice from the end.

    import Array exposing (fromList)

    fromList (List.range 0 6) |> sliceFrom 3
    --> fromList [ 3, 4, 5, 6 ]

    fromList (List.range 0 6) |> sliceFrom -3
    --> fromList [ 4, 5, 6 ]

-}
sliceFrom : Int -> (Array element -> Array element)
sliceFrom lengthDropped =
    \array ->
        array |> slice lengthDropped (array |> length)


{-| Take a number of elements from the start.
In other words, slice the `Array` from the very beginning until not including the index.
Given a negative argument, count the beginning of the slice from the end.

    import Array exposing (fromList)

    fromList (List.range 0 6) |> sliceUntil 3
    --> fromList [ 0, 1, 2 ]

    fromList (List.range 0 6) |> sliceUntil -3
    --> fromList [ 0, 1, 2, 3 ]

-}
sliceUntil : Int -> (Array element -> Array element)
sliceUntil =
    slice 0


{-| Remove the last element.

    import Array exposing (fromList, empty)

    fromList [ 1, 2, 3 ] |> pop
    --> fromList [ 1, 2 ]

    empty |> pop
    --> empty

-}
pop : Array element -> Array element
pop =
    \array -> array |> slice 0 -1


{-| Place a value between all elements.

    import Array exposing (fromList)

    fromList [ "turtles", "turtles", "turtles" ]
        |> intersperse "on"
    --> fromList
    -->     [ "turtles", "on", "turtles", "on", "turtles" ]

To interlace an `Array`, [`interweave`](#interweave).

-}
intersperse : element -> (Array element -> Array element)
intersperse separator =
    \array ->
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
filterMap :
    (element -> Maybe narrowElement)
    -> (Array element -> Array narrowElement)
filterMap tryMap =
    \array ->
        array
            |> Array.foldr
                (\el soFar -> soFar |> consTry (el |> tryMap))
                []
            |> Array.fromList


consTry : Maybe a -> List a -> List a
consTry maybeNewHead =
    \list ->
        case maybeNewHead of
            Just newHead ->
                newHead :: list

            Nothing ->
                list


{-| Apply a given `Array` of changes to all elements.
If one `Array` is longer, its extra elements are not used.

    import Array exposing (fromList, repeat)

    repeat 5 100
        |> apply
            (fromList
                [ \x -> -x, identity, (+) 10 ]
            )
    --> fromList [ -100, 100, 110 ]

-}
apply :
    Array (element -> mappedElement)
    -> (Array element -> Array mappedElement)
apply changes =
    \array ->
        array |> map2 (\map element -> map element) changes


{-| Apply a function to the elements in the array and collect the result in a List.

    import Array exposing (fromList)
    import Html

    fromList [ "a", "b", "c" ]
        |> mapToList Html.text
    --> [ Html.text "a", Html.text "b", Html.text "c" ]

-}
mapToList :
    (element -> mappedElement)
    -> (Array element -> List mappedElement)
mapToList elementChange =
    \array ->
        array
            |> Array.foldr
                (\element soFar ->
                    soFar |> (::) (element |> elementChange)
                )
                []


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
indexedMapToList :
    (Int -> element -> mappedElement)
    -> (Array element -> List mappedElement)
indexedMapToList mapIndexedElement =
    \array ->
        array
            |> Array.foldr
                (\element ( i, listSoFar ) ->
                    ( i - 1
                    , listSoFar |> (::) (mapIndexedElement i element)
                    )
                )
                ( (array |> length) - 1, [] )
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
        (aArray |> Array.toList)
        (bArray |> Array.toList)
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
    apply (map2 elementsCombine aArray bArray) cArray


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
    apply (map3 elementsCombine aArray bArray cArray) dArray


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
    apply (map4 elementsCombine aArray bArray cArray dArray) eArray


{-| Combine the elements of two `Array`s into tuples.
If one is longer, its extra elements are not used.

    import Array exposing (fromList)

    zip
        (fromList [ 1, 2, 3 ])
        (fromList [ 'a', 'b' ])
    --> fromList [ ( 1, 'a' ), ( 2, 'b' ) ]

-}
zip :
    Array firstElement
    -> Array secondElement
    -> Array ( firstElement, secondElement )
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
zip3 :
    Array firstElement
    -> Array secondElement
    -> Array thirdElement
    -> Array ( firstElement, secondElement, thirdElement )
zip3 =
    map3 (\a b c -> ( a, b, c ))


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
unzip :
    Array ( elementFirst, elementSecond )
    -> ( Array elementFirst, Array elementSecond )
unzip =
    \arrayOfTuple ->
        ( arrayOfTuple |> Array.map Tuple.first
        , arrayOfTuple |> Array.map Tuple.second
        )


{-| Only keep elements which fail to satisfy a given predicate.
This is equivalent to `Array.filter (not << predicate)`.

    import Array exposing (fromList)

    fromList [ -1, 92, 0, 14, -3 ]
        |> removeWhen (\x -> x < 0)
    --> fromList [ 92, 0, 14 ]

-}
removeWhen : (element -> Bool) -> (Array element -> Array element)
removeWhen isBad =
    \array ->
        array
            |> Array.filter (\element -> element |> isBad |> not)


{-| Resize from the left, padding the right-hand side with a given value.

    import Array exposing (fromList, empty)

    fromList [ 1, 2 ] |> resizelRepeat 4 0
    --> fromList [ 1, 2, 0, 0 ]

    fromList [ 1, 2, 3 ] |> resizelRepeat 2 0
    --> fromList [ 1, 2 ]

    fromList [ 1, 2 ] |> resizelRepeat -1 0
    --> empty

-}
resizelRepeat : Int -> element -> (Array element -> Array element)
resizelRepeat lengthNew padding =
    if lengthNew <= 0 then
        \_ -> Array.empty

    else
        \array ->
            let
                arrayLength =
                    array |> length
            in
            case compare arrayLength lengthNew of
                GT ->
                    array |> sliceUntil lengthNew

                LT ->
                    append array (repeat (lengthNew - arrayLength) padding)

                EQ ->
                    array


{-| Resize from the right, padding the left-hand side with a given value.

    import Array exposing (fromList, empty)

    fromList [ 1, 2 ] |> resizerRepeat 4 0
    --> fromList [ 0, 0, 1, 2 ]

    fromList [ 1, 2, 3 ] |> resizerRepeat 2 0
    --> fromList [ 2, 3 ]

    fromList [ 1, 2 ] |> resizerRepeat -1 0
    --> empty

-}
resizerRepeat : Int -> element -> (Array element -> Array element)
resizerRepeat lengthNew defaultValue =
    \array ->
        let
            arrayLength =
                array |> length
        in
        case compare arrayLength lengthNew of
            GT ->
                array |> slice (arrayLength - lengthNew) arrayLength

            LT ->
                append
                    (repeat (lengthNew - arrayLength) defaultValue)
                    array

            EQ ->
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
        ('a' |> Char.toCode) + inAlphabet
            |> Char.fromCode

-}
resizelIndexed :
    Int
    -> (Int -> element)
    -> (Array element -> Array element)
resizelIndexed lengthNew paddingElementForIndex =
    \array ->
        if lengthNew <= 0 then
            Array.empty

        else
            let
                arrayLength =
                    array |> length
            in
            case compare arrayLength lengthNew of
                GT ->
                    array |> sliceUntil lengthNew

                LT ->
                    append array
                        (initialize (lengthNew - arrayLength)
                            (\padIndex ->
                                (arrayLength + padIndex)
                                    |> paddingElementForIndex
                            )
                        )

                EQ ->
                    array


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
resizerIndexed :
    Int
    -> (Int -> element)
    -> (Array element -> Array element)
resizerIndexed lengthNew paddingAtIndex =
    \array ->
        let
            arrayLength =
                array |> length
        in
        case compare arrayLength lengthNew of
            GT ->
                array |> slice (arrayLength - lengthNew) arrayLength

            LT ->
                append
                    (initialize (lengthNew - arrayLength) paddingAtIndex)
                    array

            EQ ->
                array


{-| Flip the element order.

    import Array exposing (fromList)

    fromList [ 1, 2, 3, 4 ] |> reverse
    --> fromList [ 4, 3, 2, 1 ]

-}
reverse : Array element -> Array element
reverse =
    \array ->
        array
            |> reverseToList
            |> Array.fromList


reverseToList : Array element -> List element
reverseToList =
    \array -> array |> Array.foldl (::) []


{-| Split into two `Array`s, the first ending before and the second starting with a given index.

    import Array exposing (fromList, empty)

    fromList [ 1, 2, 3, 4 ] |> splitAt 2
    --> ( fromList [ 1, 2 ], fromList [ 3, 4 ] )

    fromList [ 1, 2, 3, 4 ] |> splitAt 100
    --> ( fromList [ 1, 2, 3, 4 ], empty )

    fromList [ 1, 2, 3, 4 ] |> splitAt -1
    --> ( empty, fromList [ 1, 2, 3, 4 ] )

-}
splitAt : Int -> Array element -> ( Array element, Array element )
splitAt index =
    \array ->
        if index >= 1 then
            ( array |> sliceUntil index
            , array |> sliceFrom index
            )

        else
            ( empty, array )


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
removeAt : Int -> (Array element -> Array element)
removeAt index =
    \array ->
        if index >= 0 then
            let
                ( beforeIndex, startingAtIndex ) =
                    array |> splitAt index

                lengthStartingAtIndex =
                    length startingAtIndex
            in
            if lengthStartingAtIndex == 0 then
                beforeIndex

            else
                append beforeIndex
                    (slice 1 lengthStartingAtIndex startingAtIndex)

        else
            array


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
insertAt : Int -> element -> (Array element -> Array element)
insertAt index elementToInsert =
    \array ->
        if index >= 0 then
            let
                arrayLength =
                    array |> length
            in
            if index <= arrayLength then
                let
                    before =
                        array |> Array.slice 0 index

                    after =
                        array |> Array.slice index arrayLength
                in
                Array.append (before |> Array.push elementToInsert) after

            else
                array

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
all : (element -> Bool) -> (Array element -> Bool)
all isOkay =
    \array ->
        array
            |> Array.foldl
                (\element soFar -> soFar && isOkay element)
                True


{-| Whether at least some elements satisfy a given test.

    import Array exposing (fromList, empty)

    fromList [ 6, 3 ] |> any (\x -> x < 5)
    --> True

    fromList [ 12, 33 ] |> any (\x -> x < 5)
    --> False

    empty |> any (\x -> x < 5)
    --> False

-}
any : (element -> Bool) -> (Array element -> Bool)
any isOkay =
    \array ->
        array
            |> Array.foldl
                (\element soFar -> soFar || isOkay element)
                False


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
member : element -> (Array element -> Bool)
member needle =
    \array ->
        array |> any (\element -> element == needle)


{-| Place all elements of a given `Array` between all current elements.
Extra elements of either `Array` are glued to the end without anything in between.

    import Array exposing (fromList, repeat)

    fromList [ "turtles", "turtles", "turtles" ]
        |> interweave (repeat 2 "on")
    --> fromList [ "turtles", "on", "turtles", "on", "turtles" ]

    fromList [ "turtles", "turtles", "turtles" ]
        |> interweave (repeat 5 "on")
    --> fromList [ "turtles", "on", "turtles", "on", "turtles", "on", "on", "on" ]

    fromList [ "turtles", "turtles", "turtles" ]
        |> interweave (repeat 1 "on")
    --> fromList [ "turtles", "on", "turtles", "turtles" ]

-}
interweave : Array element -> (Array element -> Array element)
interweave toInterweave =
    \array ->
        let
            untilArrayEnd =
                array
                    |> Array.foldl
                        (\element soFar ->
                            case soFar.toInterweave of
                                [] ->
                                    { interwoven =
                                        soFar.interwoven |> (::) element
                                    , toInterweave = []
                                    }

                                toInterweaveHead :: toInterweaveTail ->
                                    { interwoven =
                                        soFar.interwoven
                                            |> (::) element
                                            |> (::) toInterweaveHead
                                    , toInterweave = toInterweaveTail
                                    }
                        )
                        { interwoven = []
                        , toInterweave = toInterweave |> Array.toList
                        }
        in
        (untilArrayEnd.interwoven
            |> List.reverse
        )
            ++ untilArrayEnd.toInterweave
            |> Array.fromList
