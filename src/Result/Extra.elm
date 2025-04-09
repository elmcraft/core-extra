module Result.Extra exposing
    ( isOk, isErr, extract, unwrap, unpack, error, mapBoth, merge, join, partition, filter
    , combine, combineMap, combineArray, combineMapArray, combineFirst, combineSecond, combineBoth, combineMapFirst, combineMapSecond, combineMapBoth
    , filterArray, filterList, foldl, foldlArray, map2, map2Array
    , andMap
    , or, orLazy, orElseLazy, orElse
    , toTask
    )

{-| Convenience functions for working with `Result`.


# Common Helpers

@docs isOk, isErr, extract, unwrap, unpack, error, mapBoth, merge, join, partition, filter


# Combining

@docs combine, combineMap, combineArray, combineMapArray, combineFirst, combineSecond, combineBoth, combineMapFirst, combineMapSecond, combineMapBoth


# Containers (`List` / `Array`)

`Result`'s `andThen` behavior (monadic) is that it terminates execution immediately on first error. Projecting that into various operations on lists and arrays yields operations that terminate on first error.

@docs filterArray, filterList, foldl, foldlArray, map2, map2Array


# Applying

@docs andMap


# Alternatives

@docs or, orLazy, orElseLazy, orElse


# Conversions

@docs toTask

-}

import Array
import Task exposing (Task)


{-| Check whether the result is `Ok` without unwrapping it.
-}
isOk : Result e a -> Bool
isOk x =
    case x of
        Ok _ ->
            True

        Err _ ->
            False


{-| Check whether the result is `Err` without unwrapping it.
-}
isErr : Result e a -> Bool
isErr x =
    case x of
        Ok _ ->
            False

        Err _ ->
            True


{-| Turn a `Result e a` to an `a`, by applying the conversion
function specified to the `e`.
-}
extract : (e -> a) -> Result e a -> a
extract f x =
    case x of
        Ok a ->
            a

        Err e ->
            f e


{-| Convert a `Result e a` to a `b` by applying a function if
the `Result` is `Ok` or using the provided default value if it
is an `Err`.
-}
unwrap : b -> (a -> b) -> Result e a -> b
unwrap defaultValue okFunc result =
    case result of
        Ok ok ->
            okFunc ok

        Err _ ->
            defaultValue


{-| Convert a `Result e a` to a `b` by applying either the first
function if the `Result` is an `Err` or the second function if the
`Result` is `Ok`. Both of these functions must return the same type.
-}
unpack : (e -> b) -> (a -> b) -> Result e a -> b
unpack errFunc okFunc result =
    case result of
        Ok ok ->
            okFunc ok

        Err err ->
            errFunc err


{-| Convert to a Maybe containing the error, if there is one.

    parseInt : String -> Result ParseError Int

    maybeParseError : String -> Maybe ParseError
    maybeParseError string =
        error (parseInt string)

-}
error : Result e a -> Maybe e
error result =
    case result of
        Ok _ ->
            Nothing

        Err err ->
            Just err


{-| Apply the first argument function to an `Err` and the second
argument function to an `Ok` of a `Result`.
-}
mapBoth : (e -> f) -> (a -> b) -> Result e a -> Result f b
mapBoth errFunc okFunc result =
    case result of
        Ok ok ->
            Ok <| okFunc ok

        Err err ->
            Err <| errFunc err



-- Combining


{-| Combine a list of results into a single result (holding a list).
Also known as `sequence` on lists.
-}
combine : List (Result x a) -> Result x (List a)
combine list =
    combineHelp list []


combineHelp : List (Result x a) -> List a -> Result x (List a)
combineHelp list acc =
    case list of
        head :: tail ->
            case head of
                Ok a ->
                    combineHelp tail (a :: acc)

                Err x ->
                    Err x

        [] ->
            Ok (List.reverse acc)


{-| Filter a list using a predicate function that can fail.
-}
filterList : (a -> Result error Bool) -> List a -> Result error (List a)
filterList pred =
    let
        go : List a -> List a -> Result error (List a)
        go returnList current =
            case current of
                [] ->
                    Ok <| List.reverse returnList

                head :: tail ->
                    case pred head of
                        Err err ->
                            Err err

                        Ok b ->
                            go
                                (if b then
                                    head :: returnList

                                 else
                                    returnList
                                )
                                tail
    in
    go []


{-| Filter an array using a predicate function that can fail.
-}
filterArray : (a -> Result error Bool) -> Array.Array a -> Result error (Array.Array a)
filterArray pred arr =
    let
        go : List a -> Int -> Result error (Array.Array a)
        go returnList index =
            case Array.get index arr of
                Nothing ->
                    Ok <| Array.fromList <| List.reverse returnList

                Just head ->
                    case pred head of
                        Err err ->
                            Err err

                        Ok b ->
                            go
                                (if b then
                                    head :: returnList

                                 else
                                    returnList
                                )
                                (index + 1)
    in
    go [] 0


{-| Map two lists together with a function that produces a Result and which terminates on the first `Err` in either list.

Known as `zipWithM` in Haskell / PureScript.

-}
map2 : (a -> b -> Result e c) -> List a -> List b -> Result e (List c)
map2 zip aList bList =
    let
        go : Result e (List c) -> List a -> List b -> Result e (List c)
        go resultList a b =
            case resultList of
                Err _ ->
                    resultList

                Ok list ->
                    case ( a, b ) of
                        ( aHead :: aTail, bHead :: bTail ) ->
                            go (zip aHead bHead |> Result.map (\c -> c :: list)) aTail bTail

                        _ ->
                            Ok <| List.reverse list
    in
    go (Ok []) aList bList


{-| Map two arrays together with a function that produces a `Result` and which terminates on the first `Err` in either array.

Known as `zipWithM` in Haskell / PureScript.

-}
map2Array : (a -> b -> Result e c) -> Array.Array a -> Array.Array b -> Result e (Array.Array c)
map2Array zip aArray bArray =
    let
        go : Result e (Array.Array c) -> Int -> Result e (Array.Array c)
        go resultArray index =
            case resultArray of
                Err _ ->
                    resultArray

                Ok array ->
                    case ( Array.get index aArray, Array.get index bArray ) of
                        ( Just aHead, Just bHead ) ->
                            go (zip aHead bHead |> Result.map (\c -> Array.push c array)) (index + 1)

                        _ ->
                            resultArray
    in
    go (Ok <| Array.empty) 0


{-| Like `List.foldl` but the step function produces a `Result` and the folding terminates the moment an `Err` is encountered. This function provides early termination like `List.Extra.stoppableFoldl` but has
the following benefits

  - `Step state` used by `stoppableFoldl` supports only a **one** type for both the `Stop` and `Continue` cases. This function uses `Result` such that the terminated type (`Err terminated`) can differ from the continue type (`Ok state`).
  - By using `Result` this function has improved ergonomics as `Result` is a rich type used throughout all of Elm. It is very likely you already have functions that return `Result` or return `Maybe` which can be converted easily to `Result` with `Result.fromMaybe`.

One can think of `foldl` as a functional for-loop where the `accumulator` is some local state that will be read and returned (likely updated) on each iteration over the container (`List`). By returning `Result` and
supporting early termination this is like a for-loop with a break or early exit condition.

Similar to `foldM :: (Foldable foldable, Monad m) => (b -> a -> m b) -> b -> foldable a -> m b` in Haskell / PureScript where the `m` is `Result` and `foldable` is `List`.

-}
foldl : (item -> state -> Result terminated state) -> state -> List item -> Result terminated state
foldl step initialState =
    let
        go : Result terminated state -> List item -> Result terminated state
        go stateResult list =
            case stateResult of
                Err _ ->
                    stateResult

                Ok state ->
                    case list of
                        [] ->
                            stateResult

                        head :: rest ->
                            go (step head state) rest
    in
    go (Ok initialState)


{-| Like `Array.foldl` except that the step function produces a `Result` and the folding terminates the moment an `Err` is encountered.

One can think of `foldl` as a functional for-loop where the `accumulator` is some local state that will be read and returned (likely updated) on each iteration over the container (`Array`). By returning `Result` and
supporting early termination this is like a for-loop with a break or early exit condition.

Similar to `foldM :: (Foldable foldable, Monad m) => (b -> a -> m b) -> b -> foldable a -> m b` in Haskell / PureScript where the `m` is `Result` and `foldable` is `Array`.

-}
foldlArray : (item -> state -> Result terminated state) -> state -> Array.Array item -> Result terminated state
foldlArray step initialState arr =
    let
        go : Result terminated state -> Int -> Result terminated state
        go stateResult index =
            case stateResult of
                Err _ ->
                    stateResult

                Ok currentState ->
                    case Array.get index arr of
                        Nothing ->
                            stateResult

                        Just head ->
                            go (step head currentState) (index + 1)
    in
    go (Ok initialState) 0


{-| Map a function producing results on a list
and combine those into a single result (holding a list).
Also known as `traverse` on lists.

    combineMap f xs == combine (List.map f xs)

-}
combineMap : (a -> Result x b) -> List a -> Result x (List b)
combineMap f ls =
    combineMapHelp f ls []


combineMapHelp : (a -> Result x b) -> List a -> List b -> Result x (List b)
combineMapHelp f list acc =
    case list of
        head :: tail ->
            case f head of
                Ok a ->
                    combineMapHelp f tail (a :: acc)

                Err x ->
                    Err x

        [] ->
            Ok (List.reverse acc)


{-| Like [`combine`](#combine),
but works on [`Array`](https://package.elm-lang.org/packages/elm/core/latest/Array) instead of `List`.
-}
combineArray : Array.Array (Result x a) -> Result x (Array.Array a)
combineArray =
    Array.foldl (Result.map2 Array.push) (Ok Array.empty)


{-| Like [`combineMap`](#combineMap),
but works on [`Array`](https://package.elm-lang.org/packages/elm/core/latest/Array) instead of `List`.
-}
combineMapArray : (a -> Result x b) -> Array.Array a -> Result x (Array.Array b)
combineMapArray f =
    Array.foldl (\x -> Result.map2 Array.push (f x)) (Ok Array.empty)


{-| Pull a result out of the _first_ element of a tuple
and combine it into a result holding the tuple's values.
-}
combineFirst : ( Result x a, c ) -> Result x ( a, c )
combineFirst ( rx, y ) =
    Result.map (\x -> ( x, y )) rx


{-| Pull a result out of the _second_ element of a tuple
and combine it into a result holding the tuple's values.
Also known as `sequence` on tuples.
-}
combineSecond : ( c, Result x a ) -> Result x ( c, a )
combineSecond ( x, ry ) =
    Result.map (Tuple.pair x) ry


{-| Combine all results in a tuple
into a single result holding the tuple's values.
Also know as `bisequence` on tuples.
-}
combineBoth : ( Result x a, Result x b ) -> Result x ( a, b )
combineBoth ( rx, ry ) =
    Result.map2 Tuple.pair rx ry


{-| Map a function producing results on the _first_ element of a tuple
and then pull it out using `combineFirst`.
Also know as `sequence` on tuples.

    combineMapFirst f ( x, y )
        == combineFirst (Tuple.mapFirst f ( x, y ))
        == Result.map (\newX -> ( newX, y )) (f x)

-}
combineMapFirst : (a -> Result x b) -> ( a, c ) -> Result x ( b, c )
combineMapFirst f t =
    combineFirst (Tuple.mapFirst f t)


{-| Map a function producing results on the _second_ element of a tuple
and then pull it out using `combineSecond`.
Also know as `traverse` on tuples.

    combineMapSecond f ( x, y )
        == combineSecond (Tuple.mapSecond f ( x, y ))
        == Result.map (Tuple.pair x) (f y)

-}
combineMapSecond : (a -> Result x b) -> ( c, a ) -> Result x ( c, b )
combineMapSecond f t =
    combineSecond (Tuple.mapSecond f t)


{-| Map a function producing results on the _both_ elements of a tuple
and then pull them out using `combineBoth`.
Also know as `bitraverse` on tuples.

    combineMapBoth f g ( x, y )
        == combineBoth (Tuple.mapBoth f g ( x, y ))
        == Result.map2 Tuple.pair (f x) (g y)

-}
combineMapBoth : (a -> Result x c) -> (b -> Result x d) -> ( a, b ) -> Result x ( c, d )
combineMapBoth f g t =
    combineBoth (Tuple.mapBoth f g t)



-- Applying


{-| Apply the function that is inside `Result` to a value that is inside
`Result`. Return the result inside `Result`. If one of the `Result`
arguments is `Err e`, return `Err e`. Also known as `apply`.

    Err "Oh" |> andMap (Err "No!") == Err "Oh"

    Err "Oh" |> andMap (Ok 2) == Err "Oh"

    Ok ((+) 1) |> andMap (Err "No!") == Err "No!"

    Ok ((+) 1) |> andMap (Ok 2) == Ok 3

-}
andMap : Result e a -> Result e (a -> b) -> Result e b
andMap ra rb =
    case ( ra, rb ) of
        ( _, Err x ) ->
            Err x

        ( o, Ok fn ) ->
            Result.map fn o



-- Alternatives


{-| Like the Boolean `||` this will return the first value that is
positive (`Ok`). However, unlike with `||`, both values will be
computed anyway (there is no short-circuiting).

    or (Ok 4) (Ok 5) == Ok 4

    or (Err "Oh!") (Ok 5) == Ok 5

    or (Ok 4) (Err "No!") == Ok 4

    or (Err "Oh!") (Err "No!") == Err "No!"

As the last example line shows, the second error is returned if both
results are erroneous.

-}
or : Result e a -> Result e a -> Result e a
or ra rb =
    case ra of
        Err _ ->
            rb

        Ok _ ->
            ra


{-| Non-strict version of `or`. The second argument will only be
evaluated if the first argument is an `Err`.
-}
orLazy : Result e a -> (() -> Result e a) -> Result e a
orLazy ra frb =
    case ra of
        Err _ ->
            frb ()

        Ok _ ->
            ra


{-| Piping-friendly version of `orLazy`. The first argument will only
be evaluated if the second argument is an `Err`. Example use:

    String.toInt "Hello"
        |> orElseLazy (\() -> String.toInt "42")

-}
orElseLazy : (() -> Result e a) -> Result e a -> Result e a
orElseLazy fra rb =
    orLazy rb fra


{-| Strict version of `orElseLazy` (and at the same time,
piping-friendly version of `or`).

    orElse (Ok 4) (Ok 5) == Ok 5 -- crucial difference from `or`

    orElse (Err "Oh!") (Ok 5) == Ok 5

    orElse (Ok 4) (Err "No!") == Ok 4

    orElse (Err "Oh!") (Err "No!") == Err "Oh!" -- also different from `or`

Also:

    String.toInt "Hello"
        |> orElse (String.toInt "42")

-}
orElse : Result e a -> Result e a -> Result e a
orElse ra rb =
    or rb ra



-- OTHER --


{-| Eliminate Result when error and success have been mapped to the same
type, such as a message type.

    merge (Ok 4) == 4

    merge (Err -1) == -1

More pragmatically:

    type Msg
        = UserTypedInt Int
        | UserInputError String

    msgFromInput : String -> Msg
    msgFromInput =
        String.toInt
            >> Result.mapError UserInputError
            >> Result.map UserTypedInt
            >> Result.Extra.merge

-}
merge : Result a a -> a
merge r =
    case r of
        Ok rr ->
            rr

        Err rr ->
            rr


{-| Join contained results with the same error into one result.

Usefull if you have a "result in a result":

    join <| Ok (Ok 4) == Ok 4

    join <| Ok (Err "message") == Err "message"

-}
join : Result x (Result x a) -> Result x a
join r =
    case r of
        Err x ->
            Err x

        Ok a ->
            a


{-| Partition a list of Results into two lists of values (successes
and failures), much as List.partition takes a predicate and splits
a list based on whether the predicate indicates success or failure.

    partition ( Ok 4, Err "no", Err "hi" ) == ( [ 4 ], [ "no", "hi" ] )

    partition ( Err 7.1, Ok 'k', Err 9.0, Ok 'p' ) == ( [ 'k', 'p' ], [ 7.1, 9.0 ] )

-}
partition : List (Result e a) -> ( List a, List e )
partition rs =
    List.foldr
        (\r ( succ, err ) ->
            case r of
                Ok v ->
                    ( v :: succ, err )

                Err v ->
                    ( succ, v :: err )
        )
        ( [], [] )
        rs


{-| Take a `Result` and a predicate function and return a `Result` with the
original value when a predicate matches.

    filter "is not 1" (\v -> v == 1) (Ok 1) == Ok 1

    filter "is not 2" (\v -> v == 2) (Ok 1) == Err "is not 2"

-}
filter : e -> (a -> Bool) -> Result e a -> Result e a
filter err predicate result =
    case result of
        Ok a ->
            if predicate a then
                result

            else
                Err err

        Err _ ->
            result



-- Conversions


{-| Convert a `Result` to a `Task` that will fail or succeed immediately.

    toTask (Ok 4) == Task.succeed 4

    toTask (Err "msg") == Task.fail "msg"

This can be helpful when the value of a succeeding Task needs to be decoded, but
a failure to decode should result in a failing `Task`, not a succeeding Task
containing a `Result.Err`:

    andThenDecode : (a -> Result x b) -> Task x a -> Task x b
    andThenDecode decode =
        Task.andThen (decode >> Result.Extra.toTask)

-}
toTask : Result x a -> Task x a
toTask result =
    case result of
        Ok a ->
            Task.succeed a

        Err x ->
            Task.fail x
