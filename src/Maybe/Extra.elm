module Maybe.Extra exposing
    ( isJust, isNothing, join, filter
    , withDefaultLazy, unwrap, unpack
    , or, orElse, orList, orLazy, orElseLazy, orListLazy, oneOf
    , values
    , combine, combineMap, combineArray, combineMapArray, combineFirst, combineSecond, combineBoth, combineMapFirst, combineMapSecond, combineMapBoth
    , toList, toArray
    , cons
    , andThen2, andThen3, andThen4
    , andMap, next, prev
    )

{-| Convenience functions for [`Maybe`](https://package.elm-lang.org/packages/elm/core/latest/Maybe).


# Basics

Work with 1 `Maybe`

@docs isJust, isNothing, join, filter


# Get a value out of a `Maybe`

@docs withDefaultLazy, unwrap, unpack


# OR based logic

Take the first value that's present

@docs or, orElse, orList, orLazy, orElseLazy, orListLazy, oneOf


# Lists of `Maybe`s

@docs values


# Combining

@docs combine, combineMap, combineArray, combineMapArray, combineFirst, combineSecond, combineBoth, combineMapFirst, combineMapSecond, combineMapBoth


# Transforming to collections

@docs toList, toArray
@docs cons


# andThenN

These functions are just like [`andThen`](https://package.elm-lang.org/packages/elm/core/latest/Maybe#andThen), except they take multiple arguments.

All arguments must be `Just` and the function must return a `Just` for the maybe to be `Just`.

If you need a version of `andThenN` that takes more than 4 arguments, you can chain together [`andMap`](#andMap) calls in a pipeline.

@docs andThen2, andThen3, andThen4


# Applicative Functions

@docs andMap, next, prev

-}

import Array



-- Basics: Work with 1 `Maybe`


{-|

    isJust (Just 42)
    --> True

    isJust (Just [])
    --> True

    isJust Nothing
    --> False

-}
isJust : Maybe a -> Bool
isJust m =
    case m of
        Nothing ->
            False

        Just _ ->
            True


{-|

    isNothing (Just 42)
    --> False

    isNothing (Just [])
    --> False

    isNothing Nothing
    --> True

-}
isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Nothing ->
            True

        Just _ ->
            False


{-| Flattens nested `Maybe`s

    join (Just (Just 1))
    --> Just 1

    join (Just Nothing)
    --> Nothing

    join Nothing
    --> Nothing

-}
join : Maybe (Maybe a) -> Maybe a
join mx =
    case mx of
        Just x ->
            x

        Nothing ->
            Nothing


{-| Keep the `Maybe` only if the predicate function passes

    filter (\v -> v == 1) (Just 1)
    --> Just 1

    filter (\v -> v == 2) (Just 1)
    --> Nothing

    filter (\v -> v == 1) Nothing
    --> Nothing

-}
filter : (a -> Bool) -> Maybe a -> Maybe a
filter f m =
    case m of
        Just a ->
            if f a then
                m

            else
                Nothing

        Nothing ->
            Nothing



-- Get a value out of a `Maybe`


{-| Lazy version of [Maybe.withDefault](https://package.elm-lang.org/packages/elm/core/latest/Maybe#withDefault).

It will only calculate the default if needed.

Examples:

    withDefaultLazy (\() -> 2 + 2) Nothing
    --> 4

    withDefaultLazy (\() -> Debug.todo "Expensive calculation") (Just 4)
    --> 4

-}
withDefaultLazy : (() -> a) -> Maybe a -> a
withDefaultLazy default m =
    case m of
        Nothing ->
            default ()

        Just a ->
            a


{-| Like using a `case`.
Give a function that says what to do if the input is `Just`,
and a value to use if the input is `Nothing`.

These are all equivalent:

    unwrap default f maybeX

    maybeX
        |> Maybe.map f
        |> Maybe.withDefault default

    case maybeX of
        Just x ->
            f x

        Nothing ->
            default

Except that unlike a `case`, the default value for `unwrap` is always computed.
If your default value is expensive to compute, use the lazy [`unpack`](#unpack) instead.

Examples:

    unwrap 0 String.length Nothing
    --> 0

    unwrap 0 String.length (Just "abc")
    --> 3

-}
unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f m =
    case m of
        Nothing ->
            default

        Just a ->
            f a


{-| Like [`unwrap`](#unwrap), but the default value is lazy,
and will only be computed if the `Maybe` is `Nothing`.

    unpack (\() -> 0) String.length Nothing
    --> 0

    unpack (\() -> 0) String.length (Just "abc")
    --> 3

`unpack (\() -> default) f maybeX` is equivalent to

    case maybeX of
        Just x ->
            f x

        Nothing ->
            default

-}
unpack : (() -> b) -> (a -> b) -> Maybe a -> b
unpack default f m =
    case m of
        Nothing ->
            default ()

        Just a ->
            f a



-- Or: Combine 2 `Maybe`s


{-| Returns the first value that is present, like the boolean `||`.

Both values will be computed. There is no short-circuiting.
If your second argument is expensive to calculate and you need short circuiting, use [`orLazy`](#orLazy) instead.

    or (Just 4) (Just 5)
    --> Just 4

    or (Just 4) Nothing
    --> Just 4

    or Nothing (Just 5)
    --> Just 5

    or Nothing Nothing
    --> Nothing

Advanced functional programmers will recognize this as the
implementation of `mplus` for `Maybe`s from the `MonadPlus` type
class.

-}
or : Maybe a -> Maybe a -> Maybe a
or ma mb =
    case ma of
        Nothing ->
            mb

        Just _ ->
            ma


{-| Piping-friendly version of [`or`](#or).

    Just 5
        |> orElse (Just 4)
    --> Just 5

    orElse (Just 4) (Just 5)
    --> Just 5

    List.head []
        |> orElse (List.head [ 4 ])
    --> Just 4

-}
orElse : Maybe a -> Maybe a -> Maybe a
orElse ma mb =
    case mb of
        Nothing ->
            ma

        Just _ ->
            mb


{-| Returns the first value that is present.

All values will be computed.
If your arguments are expensive to calculate, use [`orListLazy`](#orListLazy) instead.

    orList
        [ Nothing
        , Just 1
        , Just 2
        ]
    --> Just 1

    orList
        [ List.head []
        , String.toInt ""
        ]
    --> Nothing

    orList []
    --> Nothing

-}
orList : List (Maybe a) -> Maybe a
orList maybes =
    case maybes of
        [] ->
            Nothing

        Nothing :: rest ->
            orList rest

        (Just answer) :: _ ->
            Just answer


{-| Lazy version of [`or`](#or).

The second argument will only be evaluated if the first argument is `Nothing`.

    orLazy (Just 4) (\() -> Debug.todo "Expensive calculation")
    --> Just 4

-}
orLazy : Maybe a -> (() -> Maybe a) -> Maybe a
orLazy ma fmb =
    case ma of
        Nothing ->
            fmb ()

        Just _ ->
            ma


{-| Lazy version of [`orElse`](#orElse).
Piping-friendly version of [`orLazy`](#orLazy).

The first argument will only be evaluated if the second argument is `Nothing`.

    Just 4
        |> orElseLazy (\() -> Debug.todo "Expensive calculation")
    --> Just 4

-}
orElseLazy : (() -> Maybe a) -> Maybe a -> Maybe a
orElseLazy fma mb =
    case mb of
        Nothing ->
            fma ()

        Just _ ->
            mb


{-| Lazy version of [`orList`](#orList)

Stops calculating new values after the first match

    orListLazy
        [ \() -> Nothing
        , \() -> Just 1
        , \() -> Debug.todo "Expensive calculation"
        ]
    --> Just 1

-}
orListLazy : List (() -> Maybe a) -> Maybe a
orListLazy maybes =
    oneOf maybes ()


{-| Try a list of functions against a value. Return the value of the first call that succeeds (returns `Just`).

    type UserInput
        = FloatInput Float
        | IntInput Int
        | UnknownInput

    "5.6"
        |> oneOf
            [ String.toInt >> Maybe.map IntInput
            , String.toFloat >> Maybe.map FloatInput
            ]
        |> Maybe.withDefault UnknownInput
    --> FloatInput 5.6

-}
oneOf : List (a -> Maybe b) -> a -> Maybe b
oneOf fmbs a =
    case fmbs of
        [] ->
            Nothing

        fmb :: rest ->
            case fmb a of
                Just b ->
                    Just b

                Nothing ->
                    oneOf rest a



-- Lists of `Maybe`s


{-| Take all the values that are present, throwing away any `Nothing`s.

Equivalent to [`List.filterMap identity`](https://package.elm-lang.org/packages/elm/core/latest/List#filterMap).

    values [ Just 1, Nothing, Just 2 ]
    --> [ 1, 2 ]

-}
values : List (Maybe a) -> List a
values =
    List.foldr cons []


{-| If every `Maybe` in the list is present, return all of the values unwrapped.
If there are any `Nothing`s, the whole function fails and returns `Nothing`.

    combine []
    --> Just []

    combine [ Just 1, Just 2, Just 3 ]
    --> Just [ 1, 2, 3 ]

    combine [ Just 1, Nothing, Just 3 ]
    --> Nothing

-}
combine : List (Maybe a) -> Maybe (List a)
combine list =
    combineHelp list []


combineHelp : List (Maybe a) -> List a -> Maybe (List a)
combineHelp list acc =
    case list of
        head :: tail ->
            case head of
                Just a ->
                    combineHelp tail (a :: acc)

                Nothing ->
                    Nothing

        [] ->
            Just (List.reverse acc)


{-| Like [`combine`](#combine), but map a function over each element of the list first.

If every function call succeeds (returns `Just`), `combineMap` will return a list.
If any function call fails (returns `Nothing`), `combineMap` will return `Nothing`.

`combine` is equivalent to `combineMap identity`.

    combineMap (\x -> Just (x * 10)) [ 1, 2, 3, 4, 5 ]
    --> Just [ 10, 20, 30, 40, 50 ]

    combineMap List.head [ [1], [2, 3], [] ]
    --> Nothing

-}
combineMap : (a -> Maybe b) -> List a -> Maybe (List b)
combineMap f list =
    combineMapHelp f list []


combineMapHelp : (a -> Maybe b) -> List a -> List b -> Maybe (List b)
combineMapHelp f list acc =
    case list of
        head :: tail ->
            case f head of
                Just a ->
                    combineMapHelp f tail (a :: acc)

                Nothing ->
                    Nothing

        [] ->
            Just (List.reverse acc)


{-| Like [`combine`](#combine),
but works on [`Array`](https://package.elm-lang.org/packages/elm/core/latest/Array) instead of `List`.
-}
combineArray : Array.Array (Maybe a) -> Maybe (Array.Array a)
combineArray =
    Array.foldl (Maybe.map2 Array.push) (Just Array.empty)


{-| Like [`combineMap`](#combineMap),
but works on [`Array`](https://package.elm-lang.org/packages/elm/core/latest/Array) instead of `List`.
-}
combineMapArray : (a -> Maybe b) -> Array.Array a -> Maybe (Array.Array b)
combineMapArray f =
    Array.foldl (\x -> Maybe.map2 Array.push (f x)) (Just Array.empty)


{-| Pull a maybe out of the _first_ element of a tuple
and combine it into a maybe holding the tuple's values.
-}
combineFirst : ( Maybe a, c ) -> Maybe ( a, c )
combineFirst ( mx, y ) =
    Maybe.map (\x -> ( x, y )) mx


{-| Pull a result out of the _second_ element of a tuple
and combine it into a result holding the tuple's values.
Also known as `sequence` on tuples.
-}
combineSecond : ( c, Maybe a ) -> Maybe ( c, a )
combineSecond ( x, my ) =
    Maybe.map (Tuple.pair x) my


{-| Combine all maybes in a tuple
into a single maybe holding the tuple's values.
-}
combineBoth : ( Maybe a, Maybe b ) -> Maybe ( a, b )
combineBoth ( mx, my ) =
    Maybe.map2 Tuple.pair mx my


{-| Map a function producing maybes on the _first_ element of a tuple
and then pull it out using `combineFirst`.

    combineMapFirst f ( x, y )
        == combineFirst (Tuple.mapFirst f ( x, y ))
        == Maybe.map (\newX -> ( newX, y )) (f x)

-}
combineMapFirst : (a -> Maybe b) -> ( a, c ) -> Maybe ( b, c )
combineMapFirst f t =
    combineFirst (Tuple.mapFirst f t)


{-| Map a function producing maybes on the _second_ element of a tuple
and then pull it out using `combineSecond`.

    combineMapSecond f ( x, y )
        == combineSecond (Tuple.mapSecond f ( x, y ))
        == Maybe.map (Tuple.pair x) (f y)

-}
combineMapSecond : (a -> Maybe b) -> ( c, a ) -> Maybe ( c, b )
combineMapSecond f t =
    combineSecond (Tuple.mapSecond f t)


{-| Map a function producing maybes on the _both_ elements of a tuple
and then pull them out using `combineBoth`.

    combineMapBoth f g ( x, y )
        == combineBoth (Tuple.mapBoth f g ( x, y ))
        == Maybe.map2 Tuple.pair (f x) (g y)

-}
combineMapBoth : (a -> Maybe c) -> (b -> Maybe d) -> ( a, b ) -> Maybe ( c, d )
combineMapBoth f g t =
    combineBoth (Tuple.mapBoth f g t)



-- toList


{-| A `Maybe` is a lot like a list that can only be length 0 or 1.

Returns a singleton list if the value is present, and an empty list it's missing.

    toList Nothing
    --> []

    toList (Just 1)
    --> [ 1 ]

-}
toList : Maybe a -> List a
toList m =
    case m of
        Nothing ->
            []

        Just x ->
            [ x ]


{-| Like `toList`, but returns a singleton or empty [`Array`](https://package.elm-lang.org/packages/elm/core/latest/Array).

    import Array

    toArray Nothing
    --> Array.fromList []

    toArray (Just 1)
    --> Array.fromList [ 1 ]

-}
toArray : Maybe a -> Array.Array a
toArray m =
    case m of
        Nothing ->
            Array.empty

        Just x ->
            Array.repeat 1 x


{-| Add an item to a list only if it's a `Just`.

    cons (Just 1) [ 2, 3 ]
    --> [ 1, 2, 3 ]

    cons Nothing [2, 3 ]
    --> [ 2, 3 ]

-}
cons : Maybe a -> List a -> List a
cons item list =
    case item of
        Just v ->
            v :: list

        Nothing ->
            list



-- andThenN


{-|

    import Array exposing (Array)

    array : Array Int
    array = Array.fromList [1,2,3]

    andThen2 Array.get (Just 1) (Just array)
    --> Just 2

    andThen2 Array.get Nothing (Just array)
    --> Nothing

    andThen2 Array.get (Just 1) Nothing
    --> Nothing

    andThen2 Array.get (Just 4) (Just array)
    --> Nothing

-}
andThen2 : (a -> b -> Maybe value) -> Maybe a -> Maybe b -> Maybe value
andThen2 func ma mb =
    case ma of
        Just a ->
            case mb of
                Just b ->
                    func a b

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| `andThen` for 3 maybes.
-}
andThen3 : (a -> b -> c -> Maybe value) -> Maybe a -> Maybe b -> Maybe c -> Maybe value
andThen3 func ma mb mc =
    case ma of
        Just a ->
            case mb of
                Just b ->
                    case mc of
                        Just c ->
                            func a b c

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| `andThen` for 4 maybes.
-}
andThen4 : (a -> b -> c -> d -> Maybe value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe value
andThen4 func ma mb mc md =
    case ma of
        Just a ->
            case mb of
                Just b ->
                    case mc of
                        Just c ->
                            case md of
                                Just d ->
                                    func a b c d

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing



-- Applicative Functions


{-| If both a function and a value are present, apply the function to the value.
If either argument is `Nothing`, return `Nothing`.

    Just ((+) 2)
        |> andMap (Just 3)
    --> Just 5

    Nothing
        |> andMap (Just 3)
    --> Nothing

    Just ((+) 2)
        |> andMap Nothing
    --> Nothing

This can be used to do [`Maybe.mapN`](https://package.elm-lang.org/packages/elm/core/latest/Maybe#map2) or [`andThenN`](#andthenn) for any number of arguments.

    -- map4
    Just (\a b c d -> a + b + c + d )
        |> andMap (Just 1)
        |> andMap (Just 2)
        |> andMap (Just 4)
        |> andMap (Just 8)
    --> Just 15

    -- andThen4
    Just (\a b c d -> Just (a + b + c + d ))
        |> andMap (Just 1)
        |> andMap (Just 2)
        |> andMap (Just 4)
        |> andMap (Just 8)
        |> join
    --> Just 15

Advanced functional programmers will recognize this as the implementation of `<*>` for `Maybe`s from the `Applicative` typeclass.

-}
andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap ma mb =
    case ma of
        Just o ->
            case mb of
                Just fn ->
                    Just (fn o)

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| Take two `Maybe` values. If the first one equals `Nothing`, return `Nothing`. Otherwise return the second value.

    next (Just 1) (Just 2)
    --> Just 2

    next Nothing (Just 2)
    --> Nothing

    next (Just 1) Nothing
    --> Nothing

Advanced functional programmers will recognize this as the implementation of `*>` for `Maybe`s from the `Applicative` typeclass.

-}
next : Maybe a -> Maybe b -> Maybe b
next a b =
    case a of
        Nothing ->
            Nothing

        Just _ ->
            b


{-| Take two `Maybe` values. If the second one equals `Nothing`, return `Nothing`. Otherwise return the first value.

    prev (Just 1) (Just 2)
    --> Just 1

    prev Nothing (Just 2)
    --> Nothing

    prev (Just 1) Nothing
    --> Nothing

Advanced functional programmers will recognize this as the implementation of `<*` for `Maybe`s from the `Applicative` typeclass.

-}
prev : Maybe a -> Maybe b -> Maybe a
prev a b =
    case b of
        Nothing ->
            Nothing

        Just _ ->
            a
