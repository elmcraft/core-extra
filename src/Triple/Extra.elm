module Triple.Extra exposing
    ( Triple
    , triple, from
    , first, second, third
    , apply, join, joinBy, sum, product, sort, sortBy, sortWith
    , map, mapFirst, mapSecond, mapThird
    , toList, fromListHead
    , getAA, getAB, getAC, getBA, getBB, getBC, getCA, getCB, getCC
    , getAAA, getAAB, getAAC, getABA, getABB, getABC, getACA, getACB, getACC, getBAA, getBAB, getBAC, getBBA, getBBB, getBBC, getBCA, getBCB, getBCC, getCAA, getCAB, getCAC, getCBA, getCBB, getCBC, getCCA, getCCB, getCCC
    )

{-| Convenience functions for 3-Tuples (also known as Triple).

Please remember the following from elm/core Tuple:

> For more complex data, it is best to switch to records. So instead of representing a 3D point as `(3,4,5)` and not having any helper functions, represent it as `{ x = 3, y = 4, z = 5 }` and use all the built-in record syntax!

Nonetheless, this module provides those missing helper functions.

@docs Triple


# Create

@docs triple, from


# Access

@docs first, second, third


# Manipulating

@docs apply, join, joinBy, sum, product, sort, sortBy, sortWith


# Mapping

@docs map, mapFirst, mapSecond, mapThird


# Lists

@docs toList, fromListHead


# Swizzling

Swizzling refers to getting some sort of subselection of fields into a new Tuple. Don't worry, you don't need to check the documentation, we provide all combinations of 2 and 3 fields.


## Swizzling into Pairs

@docs getAA, getAB, getAC, getBA, getBB, getBC, getCA, getCB, getCC


## Sizzling into Triples

@docs getAAA, getAAB, getAAC, getABA, getABB, getABC, getACA, getACB, getACC, getBAA, getBAB, getBAC, getBBA, getBBB, getBBC, getBCA, getBCB, getBCC, getCAA, getCAB, getCAC, getCBA, getCBB, getCBC, getCCA, getCCB, getCCC

-}

import Order.Extra



-- Types -----------------------------------------------------------------------


{-| By type aliasing Triples into a "normal" type, we remove the (small) effort
required in reading types and signatures that have Triples in. This
is most beneficial when a Triple is nested inside another type. Visually, the
Triple type is now no different to List, Maybe, or Result.

For example, this:

    List (Maybe ( String, Int, Char ))

becomes:

    List (Maybe (Triple String Int Char))

-}
type alias Triple a b c =
    ( a, b, c )


{-| Create a triple.
-}
triple : a -> b -> c -> ( a, b, c )
triple a b c =
    ( a, b, c )


{-| Occasionally you might want to create a Triple from a single value. This does
just that.

    Triple.Extra.from 1 --> ( 1, 1, 1 )

-}
from : a -> ( a, a, a )
from a =
    ( a, a, a )



-- Access ----------------------------------------------------------------------


{-| Extract the first value from a triple.
-}
first : ( a, b, c ) -> a
first ( a, _, _ ) =
    a


{-| Extract the second value from a triple.
-}
second : ( a, b, c ) -> b
second ( _, b, _ ) =
    b


{-| Extract the third value from a triple.
-}
third : ( a, b, c ) -> c
third ( _, _, c ) =
    c



-- Manipulating ----------------------------------------------------------------


{-| Given a function that takes two arguments, apply that function to the two
values contained in a Triple.

    Triple.Extra.apply (+) ( 1, 2 )
        --> 3

-}
apply : (a -> b -> c -> d) -> ( a, b, c ) -> d
apply f ( a, b, c ) =
    f a b c


{-| Similar to String.join but for Triples instead of lists. Given some separator
string, join together two strings in a Triple.

    Triple.Extra.join " " ( "Hello", "world" )
        --> "Hello world"

-}
join : appendable -> ( appendable, appendable, appendable ) -> appendable
join =
    joinBy identity identity identity


{-| Works just like join, but first converts the values of the Triple to strings.
These means the function works with any type of Triple.

    Triple.Extra.joinBy String.fromInt suitToString " of " ( 7, Club )
        == "Seven of Clubs"

-}
joinBy : (a -> appendable) -> (b -> appendable) -> (c -> appendable) -> appendable -> ( a, b, c ) -> appendable
joinBy f g h s ( a, b, c ) =
    f a ++ s ++ g b ++ s ++ h c


{-| Similar to List.sum but for Triples instead of lists. Adds together two
numbers contained in a Triple.

    Triple.Extra.sum ( 1, 2 )
        --> 3

-}
sum : ( number, number, number ) -> number
sum ( a, b, c ) =
    a + b + c


{-| Similar to List.sum but for Triples instead of lists. Multiplies together two
numbers contained in a Triple

    Triple.Extra.product ( 1, 2 )
        --> 2

-}
product : ( number, number, number ) -> number
product ( a, b, c ) =
    a * b * c


{-| Similar to List.sort but for Triples instead of lists. Sort values contained
in a Triple from lowest to highest

    Triple.Extra.sort ( 2, 1 )
        --> ( 1, 2 )

-}
sort : ( comparable, comparable, comparable ) -> ( comparable, comparable, comparable )
sort =
    sortBy identity


{-| Similar to List.sortBy but for Triples instead of lists. Sort values
contained in a Triple by first converting both values to a `comparable`. The
values are sorted lowest to highest

    Triple.Extra.sortBy String.length ( "mouse", "cat" )
        --> ( "cat", "mouse" )

-}
sortBy : (a -> comparable) -> ( a, a, a ) -> ( a, a, a )
sortBy toComparable =
    sortWith (Order.Extra.byField toComparable)


{-| Similar to List.sortWith but for Triples instead of lists. Instead of
converting values contained in a Triple to `comparable`s, instead supply a
function that will produce an `Order` directly.

    Triple.Extra.sortWith Basics.compare ( 2, 1, 3 )
        --> ( 1, 2, 3 )

-}
sortWith : (a -> a -> Order) -> ( a, a, a ) -> ( a, a, a )
sortWith toOrder ( a, b, c ) =
    case ( toOrder a b, toOrder b c ) of
        ( LT, LT ) ->
            ( a, b, c )

        ( GT, GT ) ->
            ( c, b, a )

        ( EQ, EQ ) ->
            ( a, b, c )

        ( LT, EQ ) ->
            ( a, b, c )

        ( LT, GT ) ->
            ( a, c, b )

        ( EQ, LT ) ->
            ( c, a, b )

        ( EQ, GT ) ->
            ( a, b, c )

        ( GT, LT ) ->
            ( c, b, a )

        ( GT, EQ ) ->
            ( b, c, a )



-- Mapping ---------------------------------------------------------------------


{-| Transform the first value in a triple.

    Triple.Extra.mapFirst negate ( -3, 10, '9' )
        --> ( 3, 10, '9' )

-}
mapFirst : (a -> x) -> ( a, b, c ) -> ( x, b, c )
mapFirst f ( a, b, c ) =
    ( f a, b, c )


{-| Transform the second value in a triple.

    Triple.Extra.mapSecond negate ( -3, 10, '9' )
        --> ( -3, 010, '9' )

-}
mapSecond : (b -> x) -> ( a, b, c ) -> ( a, x, c )
mapSecond f ( a, b, c ) =
    ( a, f b, c )


{-| Transform the third value in a triple.

    Triple.Extra.mapThird String.toUpper ( -3, 10, "a" )
        --> ( -3, 10, "A" )

-}
mapThird : (c -> x) -> ( a, b, c ) -> ( a, b, x )
mapThird f ( a, b, c ) =
    ( a, b, f c )


{-| Apply a function to both values contained in a Triple. This might also be
known as `mapBothWith` or `bimap`.

    Triple.Extra.map negate ( -3, 10 )
        --> ( 3, -10 )

-}
map : (a -> b) -> ( a, a, a ) -> ( b, b, b )
map f ( a, b, c ) =
    ( f a, f b, f c )



-- List conversion --------------------------------------------------------------


toList : ( a, a, a ) -> List a
toList ( a, b, c ) =
    [ a, b, c ]


fromListHead : List a -> Maybe ( a, a, a )
fromListHead lst =
    case lst of
        a :: b :: c :: _ ->
            Just ( a, b, c )

        _ ->
            Nothing



-- Swizzling ---------------------------------------------------------------------


{-| -}
getAA : ( a, b, c ) -> ( a, a )
getAA ( a, _, _ ) =
    ( a, a )


{-| -}
getAB : ( a, b, c ) -> ( a, b )
getAB ( a, b, _ ) =
    ( a, b )


{-| -}
getAC : ( a, b, c ) -> ( a, c )
getAC ( a, _, c ) =
    ( a, c )


{-| -}
getBA : ( a, b, c ) -> ( b, a )
getBA ( a, b, _ ) =
    ( b, a )


{-| -}
getBB : ( a, b, c ) -> ( b, b )
getBB ( a, b, _ ) =
    ( b, b )


{-| -}
getBC : ( a, b, c ) -> ( b, c )
getBC ( a, b, c ) =
    ( b, c )


{-| -}
getCA : ( a, b, c ) -> ( c, a )
getCA ( a, b, c ) =
    ( c, a )


{-| -}
getCB : ( a, b, c ) -> ( c, b )
getCB ( a, b, c ) =
    ( c, b )


{-| -}
getCC : ( a, b, c ) -> ( c, c )
getCC ( a, b, c ) =
    ( c, c )



-- NOTE: I codegenned the following functions using this script:
--  ch = ['A', 'B', 'C']
--  toVar a = String.toLower (String.fromList [a])
--  List.concatMap (\a ->
--      List.concatMap (\b ->
--          List.map (\c -> "{-| -}\nget" ++ String.fromList [a,b,c] ++ " : (a,b,c) -> (" ++ toVar a ++ ", " ++ toVar b ++ ", " ++ toVar c ++
--              ")\nget" ++ String.fromList [a,b,c] ++ " ( a, b, c) = (" ++ toVar a ++ ", " ++ toVar b ++ ", " ++ toVar c ++ ")"
--          ) ch)
--       ch)
--  ch
--     |> String.join "\n\n"


{-| -}
getAAA : ( a, b, c ) -> ( a, a, a )
getAAA ( a, b, c ) =
    ( a, a, a )


{-| -}
getAAB : ( a, b, c ) -> ( a, a, b )
getAAB ( a, b, c ) =
    ( a, a, b )


{-| -}
getAAC : ( a, b, c ) -> ( a, a, c )
getAAC ( a, b, c ) =
    ( a, a, c )


{-| -}
getABA : ( a, b, c ) -> ( a, b, a )
getABA ( a, b, c ) =
    ( a, b, a )


{-| -}
getABB : ( a, b, c ) -> ( a, b, b )
getABB ( a, b, c ) =
    ( a, b, b )


{-| -}
getABC : ( a, b, c ) -> ( a, b, c )
getABC =
    identity


{-| -}
getACA : ( a, b, c ) -> ( a, c, a )
getACA ( a, b, c ) =
    ( a, c, a )


{-| -}
getACB : ( a, b, c ) -> ( a, c, b )
getACB ( a, b, c ) =
    ( a, c, b )


{-| -}
getACC : ( a, b, c ) -> ( a, c, c )
getACC ( a, b, c ) =
    ( a, c, c )


{-| -}
getBAA : ( a, b, c ) -> ( b, a, a )
getBAA ( a, b, c ) =
    ( b, a, a )


{-| -}
getBAB : ( a, b, c ) -> ( b, a, b )
getBAB ( a, b, c ) =
    ( b, a, b )


{-| -}
getBAC : ( a, b, c ) -> ( b, a, c )
getBAC ( a, b, c ) =
    ( b, a, c )


{-| -}
getBBA : ( a, b, c ) -> ( b, b, a )
getBBA ( a, b, c ) =
    ( b, b, a )


{-| -}
getBBB : ( a, b, c ) -> ( b, b, b )
getBBB ( a, b, c ) =
    ( b, b, b )


{-| -}
getBBC : ( a, b, c ) -> ( b, b, c )
getBBC ( a, b, c ) =
    ( b, b, c )


{-| -}
getBCA : ( a, b, c ) -> ( b, c, a )
getBCA ( a, b, c ) =
    ( b, c, a )


{-| -}
getBCB : ( a, b, c ) -> ( b, c, b )
getBCB ( a, b, c ) =
    ( b, c, b )


{-| -}
getBCC : ( a, b, c ) -> ( b, c, c )
getBCC ( a, b, c ) =
    ( b, c, c )


{-| -}
getCAA : ( a, b, c ) -> ( c, a, a )
getCAA ( a, b, c ) =
    ( c, a, a )


{-| -}
getCAB : ( a, b, c ) -> ( c, a, b )
getCAB ( a, b, c ) =
    ( c, a, b )


{-| -}
getCAC : ( a, b, c ) -> ( c, a, c )
getCAC ( a, b, c ) =
    ( c, a, c )


{-| -}
getCBA : ( a, b, c ) -> ( c, b, a )
getCBA ( a, b, c ) =
    ( c, b, a )


{-| -}
getCBB : ( a, b, c ) -> ( c, b, b )
getCBB ( a, b, c ) =
    ( c, b, b )


{-| -}
getCBC : ( a, b, c ) -> ( c, b, c )
getCBC ( a, b, c ) =
    ( c, b, c )


{-| -}
getCCA : ( a, b, c ) -> ( c, c, a )
getCCA ( a, b, c ) =
    ( c, c, a )


{-| -}
getCCB : ( a, b, c ) -> ( c, c, b )
getCCB ( a, b, c ) =
    ( c, c, b )


{-| -}
getCCC : ( a, b, c ) -> ( c, c, c )
getCCC ( a, b, c ) =
    ( c, c, c )
